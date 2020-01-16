use codegame::prelude::*;

mod action;
mod bullet;
mod custom_data;
mod game;
mod level;
mod loot;
mod mine;
mod options;
mod properties;
mod unit;
mod weapon;

pub use action::*;
pub use bullet::*;
pub use custom_data::*;
pub use game::*;
pub use level::*;
pub use loot::*;
pub use mine::*;
pub use options::*;
pub use properties::*;
pub use unit::*;
pub use weapon::*;

#[derive(Debug, Serialize, Deserialize, Clone, Hash, PartialEq, Eq, Copy, Trans, Schematic)]
pub struct Id(usize);

impl Id {
    pub fn new() -> Self {
        static NEXT_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(1);
        Self(NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
    pub fn raw(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub struct Player {
    pub id: Id,
    pub score: usize,
}

#[derive(Debug, Serialize, Deserialize, Clone, Diff, Trans, Schematic)]
pub struct Game {
    #[diff = "clone"]
    pub current_tick: usize,
    #[diff = "eq"]
    pub properties: Properties,
    pub level: Level,
    #[diff = "clone"]
    pub players: Vec<Player>,
    #[diff = "clone"]
    pub units: Vec<Unit>,
    #[diff = "clone"]
    pub bullets: Vec<Bullet>,
    #[diff = "clone"]
    pub mines: Vec<Mine>,
    #[diff = "clone"]
    pub loot_boxes: Vec<LootBox>,
}

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub struct PlayerView {
    pub my_id: Id,
    pub game: Game,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ActionWrapper {
    V0 { actions: HashMap<Id, OldUnitAction> },
    V1 { actions: HashMap<Id, UnitAction> },
}

impl ActionWrapper {
    const MAGIC: i32 = 0xabcd;

    fn to_new(&self) -> HashMap<Id, UnitAction> {
        match self {
            Self::V0 { actions } => actions
                .clone()
                .into_iter()
                .map(|(id, action)| (id, action.into()))
                .collect(),
            Self::V1 { actions } => actions.clone(),
        }
    }
}

impl Default for ActionWrapper {
    fn default() -> Self {
        Self::V1 { actions: default() }
    }
}

impl Trans for ActionWrapper {
    fn read_from(mut reader: impl std::io::Read) -> std::io::Result<Self> {
        let x = i32::read_from(&mut reader)?;
        Ok(match x {
            ActionWrapper::MAGIC => Self::V1 {
                actions: Trans::read_from(&mut reader)?,
            },
            _ => {
                let mut actions = HashMap::new();
                for _ in 0..x {
                    let key = Id::read_from(&mut reader)?;
                    let value = OldUnitAction::read_from(&mut reader)?;
                    actions.insert(key, value);
                }
                Self::V0 { actions }
            }
        })
    }
    fn write_to(&self, mut writer: impl std::io::Write) -> std::io::Result<()> {
        unimplemented!("Not used")
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Schematic)]
#[schematic(rename = "Versioned", magic = "ActionWrapper::MAGIC")]
struct LastVersionUnitAction {
    inner: HashMap<Id, UnitAction>,
}

impl Schematic for ActionWrapper {
    fn create_schema() -> trans_schema::Schema {
        LastVersionUnitAction::create_schema()
    }
}

pub type Action = ActionWrapper;
pub type Results = Vec<usize>;

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub enum Event {
    LevelEvent {
        used_tile: Vec2<usize>,
    },
    Explosion {
        position: Vec2<R64>,
        player_id: Id,
        params: ExplosionParams,
    },
    Shot {
        weapon_type: WeaponType,
    },
    Heal,
    PickupWeapon,
    PickupMine,
    Hit,
}

impl codegame::Game for Game {
    type Options = Options;
    type OptionsPreset = crate::OptionsPreset;
    type PlayerOptions = crate::PlayerOptions;
    type PlayerExtraData = crate::PlayerExtraData;
    type Action = Action;
    type PlayerView = PlayerView;
    type Event = Event;
    type Results = Results;
    type CustomData = CustomData;

    fn init(rng: &mut dyn rand::RngCore, player_count: usize, options: Options) -> Self {
        let properties = options.properties.unwrap_or_default();
        let (level, spawn_points) = options.level.create(rng);
        let mut players = Vec::new();
        let mut units = Vec::new();
        for i in 0..player_count {
            let player = Player {
                id: Id::new(),
                score: 0,
            };
            players.push(player);
        }
        let mut spawn_points = spawn_points.into_iter();
        for _ in 0..properties.team_size {
            for player in &players {
                units.push(Unit::spawn(
                    &properties,
                    player.id,
                    spawn_points.next().unwrap(),
                ));
            }
        }
        let mut loot_boxes = Vec::new();
        {
            let mut possible_positions = Vec::new();
            for (pos, tile) in &level {
                if pos.x < level.size().x / 2
                    && pos.y > 0
                    && tile == &Tile::Empty
                    && level.get(pos.x, pos.y - 1).unwrap().is_walkable()
                    && level.get(pos.x, pos.y) == level.get(level.size().x - 1 - pos.x, pos.y)
                    && level.get(pos.x, pos.y - 1)
                        == level.get(level.size().x - 1 - pos.x, pos.y - 1)
                {
                    possible_positions.push(pos);
                }
            }
            let loot_count = possible_positions.len() / 4;
            for (i, &mut pos) in possible_positions
                .partial_shuffle(rng, loot_count)
                .0
                .into_iter()
                .enumerate()
            {
                let item = if i < 3 {
                    Item::Weapon {
                        weapon_type: match i {
                            0 => WeaponType::Pistol,
                            1 => WeaponType::AssaultRifle,
                            2 => WeaponType::RocketLauncher,
                            _ => unreachable!(),
                        },
                    }
                } else if rng.gen::<bool>() {
                    Item::HealthPack {
                        health: properties.health_pack_health,
                    }
                } else {
                    Item::Mine {}
                };
                for &pos in &[pos, vec2(level.size().x - 1 - pos.x, pos.y)] {
                    loot_boxes.push(LootBox::spawn(
                        &properties,
                        vec2(r64(pos.x as f64 + 0.5), r64(pos.y as f64)),
                        item.clone(),
                    ));
                }
            }
            let mut used_positions = HashSet::new();
            for _ in 0..15 {
                let pos = loop {
                    let pos = vec2(
                        rng.gen_range(0, level.size().x),
                        rng.gen_range(1, level.size().y),
                    );
                    if level.tiles[pos.x][pos.y] == Tile::Empty
                        && level.tiles[pos.x][pos.y - 1].is_walkable()
                        && !used_positions.contains(&pos)
                    {
                        break pos;
                    }
                };
                used_positions.insert(pos);
            }
            for (i, pos) in used_positions.into_iter().enumerate() {}
        }
        Self {
            current_tick: 0,
            properties,
            level,
            players,
            units,
            bullets: Vec::new(),
            mines: Vec::new(),
            loot_boxes,
        }
    }

    fn player_view(&self, player_index: usize) -> PlayerView {
        PlayerView {
            my_id: self.players[player_index].id,
            game: self.clone(),
        }
    }

    fn process_turn(
        &mut self,
        rng: &mut dyn rand::RngCore,
        actions: HashMap<usize, Action>,
    ) -> Vec<Self::Event> {
        let mut unit_actions = HashMap::new();
        for (player_index, action) in actions {
            let player = &self.players[player_index];
            for (unit_id, action) in action.to_new() {
                let unit = self.units.iter().find(|unit| unit.id == unit_id);
                if let Some(unit) = unit {
                    if unit.player_id != player.id {
                        warn!("Received action for a unit not owned by the player");
                        continue;
                    }
                    unit_actions.insert(unit_id, action);
                } else {
                    warn!("Received action for non-existent unit");
                }
            }
        }
        let mut events = Vec::new();
        let mut unit_swapped = HashSet::new();
        let mut unit_planted_mine = HashSet::new();
        for _ in 0..self.properties.updates_per_tick {
            let delta_time = r64(1.0)
                / self.properties.ticks_per_second
                / r64(self.properties.updates_per_tick as _);
            self.update(
                rng,
                &mut events,
                &mut unit_planted_mine,
                &mut unit_swapped,
                &unit_actions,
                delta_time,
            );
        }
        self.current_tick += 1;
        events
    }
    fn finished(&self) -> bool {
        self.units
            .iter()
            .map(|unit| unit.player_id)
            .collect::<HashSet<_>>()
            .len()
            <= 1
            || self.current_tick >= self.properties.max_tick_count
    }
    fn results(&self) -> Self::Results {
        self.players.iter().map(|player| player.score).collect()
    }
}
