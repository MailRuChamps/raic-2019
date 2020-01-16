use crate::*;

mod background;
mod bullet;
mod camera;
mod custom;
mod fancy_bullet;
mod fancy_explosion;
mod fancy_level;
mod fancy_loot;
mod fancy_mine;
mod fancy_unit;
mod fancy_weapon;
mod level;
mod loot;
mod mine;
mod simple;
mod sprite;
mod unit;
mod weapon;

use background::BackgroundRenderer;
use bullet::BulletRenderer;
pub use camera::Camera;
use custom::CustomRenderer;
use fancy_bullet::FancyBulletRenderer;
use fancy_explosion::FancyExplosionRenderer;
pub use fancy_level::FancyLevelRenderer;
use fancy_loot::FancyLootRenderer;
use fancy_mine::FancyMineRenderer;
use fancy_unit::FancyUnitRenderer;
pub use level::LevelRenderer;
use loot::LootRenderer;
use mine::MineRenderer;
pub use simple::SimpleRenderer;
use sprite::SpriteRenderer;
use unit::UnitRenderer;
use weapon::WeaponRenderer;

#[derive(geng::Assets)]
pub struct Sounds {
    #[asset(path = "explosion.wav")]
    explosion: geng::Sound,
    #[asset(path = "pickup.wav")]
    pickup: geng::Sound,
    #[asset(path = "heal.wav")]
    heal: geng::Sound,
    #[asset(path = "hit.wav")]
    hit: geng::Sound,
    #[asset(path = "guns/pistol/shoot.wav")]
    pistol_shoot: geng::Sound,
    #[asset(path = "guns/assault_rifle/shoot.wav")]
    assault_rifle_shoot: geng::Sound,
    #[asset(path = "guns/rocket_launcher/shoot.wav")]
    rocket_launcher_shoot: geng::Sound,
}

#[derive(geng::Assets)]
pub struct Assets {
    #[asset(path = "level")]
    level: fancy_level::Assets,
    #[asset(path = ".")]
    background: background::Assets,
    #[asset(path = ".")]
    explosion: fancy_explosion::Assets,
    #[asset(path = "unit")]
    unit: fancy_unit::Assets,
    #[asset(path = "guns")]
    weapon: fancy_weapon::Assets,
    #[asset(path = "loot")]
    loot: fancy_loot::Assets,
    #[asset(path = "mine")]
    mine: fancy_mine::Assets,
    #[asset(path = ".")]
    sounds: Sounds,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, Hash)]
enum Mode {
    Fancy,
    Schematic,
    Blank,
}

impl Mode {
    fn switch_next(&mut self) {
        let new_mode = match self {
            Self::Fancy => Self::Schematic,
            Self::Schematic => Self::Blank,
            Self::Blank => Self::Fancy,
        };
        *self = new_mode;
    }
}

pub struct Renderer {
    geng: Rc<Geng>,
    default_tps: f64,
    keyboard_player_input: Arc<Mutex<keyboard_player::Input>>,
    simple_renderer: Rc<SimpleRenderer>,
    sprite_renderer: Rc<SpriteRenderer>,
    camera: Camera,
    level_renderer: LevelRenderer,
    fancy_level_renderer: FancyLevelRenderer,
    unit_renderer: UnitRenderer,
    bullet_renderer: BulletRenderer,
    loot_renderer: LootRenderer,
    fancy_loot_renderer: FancyLootRenderer,
    fancy_bullet_renderer: FancyBulletRenderer,
    mine_renderer: MineRenderer,
    background: BackgroundRenderer,
    fancy_unit: FancyUnitRenderer,
    fancy_mine: FancyMineRenderer,
    fancy_explosion: FancyExplosionRenderer,
    custom: CustomRenderer,
    sounds: Sounds,
    player_names: Vec<String>,
    preferences: Rc<RefCell<AutoSave<codegame::AppPreferences<RendererPreferences>>>>,
}

impl Renderer {
    pub fn new(
        geng: &Rc<Geng>,
        preferences: Rc<RefCell<AutoSave<codegame::AppPreferences<RendererPreferences>>>>,
        keyboard_player_input: &Arc<Mutex<keyboard_player::Input>>,
        mut player_names: Vec<String>,
        mut assets: Assets,
    ) -> Self {
        assets.weapon.setup();
        let weapon_assets = Rc::new(assets.weapon);
        let simple_renderer = Rc::new(SimpleRenderer::new(geng));
        let sprite_renderer = Rc::new(SpriteRenderer::new(geng));
        let weapon_renderer = Rc::new(WeaponRenderer::new(&simple_renderer));
        if player_names.len() == 0 {
            player_names.push(format!("{} 1", translate("Player")));
        }
        if player_names.len() == 1 {
            player_names.push(format!("{} 2", translate("Player")));
        }
        Self {
            geng: geng.clone(),
            default_tps: 1.0,
            keyboard_player_input: keyboard_player_input.clone(),
            simple_renderer: simple_renderer.clone(),
            sprite_renderer: sprite_renderer.clone(),
            camera: Camera::new(),
            level_renderer: LevelRenderer::new(geng, &simple_renderer),
            fancy_level_renderer: FancyLevelRenderer::new(geng, assets.level),
            unit_renderer: UnitRenderer::new(geng, &simple_renderer, &weapon_renderer),
            bullet_renderer: BulletRenderer::new(&simple_renderer),
            loot_renderer: LootRenderer::new(&simple_renderer, &weapon_renderer),
            mine_renderer: MineRenderer::new(&simple_renderer),
            background: BackgroundRenderer::new(geng, assets.background),
            fancy_unit: FancyUnitRenderer::new(
                geng,
                &simple_renderer,
                &sprite_renderer,
                assets.unit,
                &weapon_assets,
            ),
            fancy_loot_renderer: FancyLootRenderer::new(&sprite_renderer, assets.loot),
            fancy_bullet_renderer: FancyBulletRenderer::new(&sprite_renderer, &weapon_assets),
            fancy_mine: FancyMineRenderer::new(&sprite_renderer, assets.mine),
            fancy_explosion: FancyExplosionRenderer::new(&sprite_renderer, assets.explosion),
            custom: CustomRenderer::new(geng),
            sounds: assets.sounds,
            player_names,
            preferences: preferences.clone(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Trans, PartialEq, Eq)]
pub struct Explosion {
    tick: usize,
    player_id: model::Id,
    position: Vec2<R64>,
    params: model::ExplosionParams,
}

#[derive(Clone, Diff, Serialize, Deserialize, Trans)]
pub struct RendererExtraData {
    #[diff = "eq"]
    pub level_last_used_tick: HashMap<Vec2<usize>, usize>,
    #[diff = "eq"]
    pub explosions: Vec<Explosion>,
}

impl RendererExtraData {
    const EXPLOSION_TICKS: usize = 30;
}

impl codegame::RendererExtraData<model::Game> for RendererExtraData {
    fn new(game: &model::Game) -> Self {
        Self {
            level_last_used_tick: HashMap::new(),
            explosions: Vec::new(),
        }
    }
    fn update(&mut self, events: &[model::Event], game: &model::Game) {
        for event in events {
            match event {
                model::Event::LevelEvent { used_tile } => {
                    self.level_last_used_tick
                        .insert(*used_tile, game.current_tick);
                }
                model::Event::Explosion {
                    player_id,
                    position,
                    params,
                } => {
                    self.explosions.push(Explosion {
                        tick: game.current_tick,
                        player_id: *player_id,
                        position: *position,
                        params: params.clone(),
                    });
                }
                model::Event::Shot { .. }
                | model::Event::PickupMine
                | model::Event::PickupWeapon
                | model::Event::Heal
                | model::Event::Hit => {}
            }
        }
        self.explosions
            .retain(|explosion| explosion.tick + Self::EXPLOSION_TICKS > game.current_tick);
    }
}

impl Renderer {
    const SCHEMATIC_EXPLOSION_TICKS: usize = 10;
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct RendererPreferences {
    mode: Mode,
}

impl Default for RendererPreferences {
    fn default() -> Self {
        Self { mode: Mode::Fancy }
    }
}

impl codegame::Renderer<model::Game> for Renderer {
    type ExtraData = RendererExtraData;
    type Preferences = RendererPreferences;
    fn default_tps(&self) -> f64 {
        self.default_tps
    }
    fn draw(
        &mut self,
        game: &model::Game,
        extra_data: &RendererExtraData,
        custom_data: &HashMap<usize, Vec<model::CustomData>>,
        framebuffer: &mut ugli::Framebuffer,
    ) {
        let framebuffer_size = framebuffer.size();
        self.default_tps = game.properties.ticks_per_second.raw();
        self.camera.center = game.level.size().map(|x| r64(x as _)) / r64(2.0);
        self.camera.fov = r64(game.level.size().y as f64 + 5.0);
        {
            let mut input = self.keyboard_player_input.lock().unwrap();
            input.mouse_pos = self
                .camera
                .screen_to_world(framebuffer, self.geng.window().mouse_pos().map(|x| r64(x)));
            input.pressed_keys = self.geng.window().pressed_keys().clone();
            input.pressed_buttons = self.geng.window().pressed_buttons().clone();
        }
        match self.preferences.borrow().renderer.mode {
            Mode::Fancy => {
                ugli::clear(framebuffer, Some(Color::BLACK), None);
                self.background.draw(framebuffer, game);
                self.fancy_level_renderer.draw(
                    framebuffer,
                    &self.camera,
                    &game.level,
                    Some(game),
                    Some(extra_data),
                );
                self.fancy_loot_renderer
                    .draw(framebuffer, &self.camera, game);
                self.fancy_bullet_renderer
                    .draw(framebuffer, &self.camera, game);
                self.fancy_unit.draw(framebuffer, &self.camera, game);
                self.fancy_mine.draw(framebuffer, &self.camera, game);
                self.fancy_explosion
                    .draw(framebuffer, &self.camera, &extra_data.explosions, game);
            }
            Mode::Schematic => {
                ugli::clear(framebuffer, Some(Color::BLACK), None);
                self.level_renderer.draw(
                    framebuffer,
                    &self.camera,
                    &game.level,
                    Some(game),
                    Some(extra_data),
                );
                self.loot_renderer.draw(framebuffer, &self.camera, game);
                self.bullet_renderer.draw(framebuffer, &self.camera, game);
                self.unit_renderer.draw(framebuffer, &self.camera, game);
                self.mine_renderer.draw(framebuffer, &self.camera, game);
                for explosion in &extra_data.explosions {
                    if explosion.tick + Self::SCHEMATIC_EXPLOSION_TICKS > game.current_tick {
                        let size = vec2(explosion.params.radius, explosion.params.radius);
                        self.simple_renderer.quad(
                            framebuffer,
                            &self.camera,
                            AABB::from_corners(
                                explosion.position - size,
                                explosion.position + size,
                            ),
                            Color::rgba(1.0, 0.0, 0.0, 0.5),
                        );
                    }
                }
            }
            Mode::Blank => {
                ugli::clear(framebuffer, Some(Color::BLACK), None);
            }
        }
        self.custom.draw_all(framebuffer, &self.camera, custom_data);
        let font_size = 20.0;
        let player_colors = [Color::rgb(1.0, 0.7, 0.7), Color::rgb(0.7, 0.7, 1.0)];
        if self.preferences.borrow().renderer.mode != Mode::Blank {
            for unit in &game.units {
                let player_index = game
                    .players
                    .iter()
                    .position(|player| player.id == unit.player_id)
                    .unwrap();
                if let Some(name) = self.player_names.get(player_index) {
                    let pos = self.camera.world_to_screen(
                        framebuffer,
                        vec2(unit.position.x, unit.position.y + unit.size.y),
                    );
                    self.geng.default_font().draw_aligned(
                        framebuffer,
                        name,
                        pos.map(|x| x.raw() as f32),
                        0.5,
                        20.0,
                        player_colors[player_index],
                    );
                }
            }
            let mid_width = partial_max(
                self.geng
                    .default_font()
                    .measure(translate("score"), font_size)
                    .width(),
                self.geng
                    .default_font()
                    .measure(translate("versus"), font_size)
                    .width(),
            );
            let off = mid_width / 2.0 + font_size;
            self.geng.default_font().draw_aligned(
                framebuffer,
                &format!("{}", game.players[0].score),
                vec2(
                    framebuffer_size.x as f32 / 2.0 - off,
                    framebuffer_size.y as f32 - 40.0 - font_size,
                ),
                1.0,
                font_size,
                Color::WHITE,
            );
            self.geng.default_font().draw_aligned(
                framebuffer,
                translate("score"),
                vec2(
                    framebuffer_size.x as f32 / 2.0,
                    framebuffer_size.y as f32 - 40.0 - font_size,
                ),
                0.5,
                font_size,
                Color::GRAY,
            );
            self.geng.default_font().draw_aligned(
                framebuffer,
                &format!("{}", game.players[1].score),
                vec2(
                    framebuffer_size.x as f32 / 2.0 + off,
                    framebuffer_size.y as f32 - 40.0 - font_size,
                ),
                0.0,
                font_size,
                Color::WHITE,
            );
            if self.player_names.len() == 2 {
                self.geng.default_font().draw_aligned(
                    framebuffer,
                    &self.player_names[0],
                    vec2(
                        framebuffer_size.x as f32 / 2.0 - off,
                        framebuffer_size.y as f32 - 40.0,
                    ),
                    1.0,
                    font_size,
                    player_colors[0],
                );
                self.geng.default_font().draw_aligned(
                    framebuffer,
                    translate("versus"),
                    vec2(
                        framebuffer_size.x as f32 / 2.0,
                        framebuffer_size.y as f32 - 40.0,
                    ),
                    0.5,
                    font_size,
                    Color::GRAY,
                );
                self.geng.default_font().draw_aligned(
                    framebuffer,
                    &self.player_names[1],
                    vec2(
                        framebuffer_size.x as f32 / 2.0 + off,
                        framebuffer_size.y as f32 - 40.0,
                    ),
                    0.0,
                    font_size,
                    player_colors[1],
                );
            }
        }
    }
    fn process_event(&mut self, event: &model::Event) {
        match event {
            model::Event::Explosion { .. } => {
                let mut effect = self.sounds.explosion.effect();
                effect.set_volume(self.preferences.borrow().volume);
                effect.play();
            }
            model::Event::LevelEvent { .. } => {}
            model::Event::Shot { weapon_type } => {
                let sound = match weapon_type {
                    model::WeaponType::Pistol => &self.sounds.pistol_shoot,
                    model::WeaponType::AssaultRifle => &self.sounds.assault_rifle_shoot,
                    model::WeaponType::RocketLauncher => &self.sounds.rocket_launcher_shoot,
                };
                let mut effect = sound.effect();
                effect.set_volume(self.preferences.borrow().volume);
                effect.play();
            }
            model::Event::PickupMine | model::Event::PickupWeapon => {
                let mut effect = self.sounds.pickup.effect();
                effect.set_volume(self.preferences.borrow().volume);
                effect.play();
            }
            model::Event::Heal => {
                let mut effect = self.sounds.heal.effect();
                effect.set_volume(self.preferences.borrow().volume);
                effect.play();
            }
            model::Event::Hit => {
                let mut effect = self.sounds.hit.effect();
                effect.set_volume(self.preferences.borrow().volume);
                effect.play();
            }
        }
    }
    fn handle_event(&mut self, event: &geng::Event) {
        match event {
            geng::Event::KeyDown { key, .. } => match key {
                geng::Key::M => {
                    self.preferences.borrow_mut().renderer.mode.switch_next();
                }
                _ => {}
            },
            _ => {}
        }
    }
}
