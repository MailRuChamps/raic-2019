pub struct MyStrategy {}

impl MyStrategy {
    pub fn new() -> Self {
        Self {}
    }
    pub fn get_action(
        &mut self,
        unit: &model::Unit,
        game: &model::Game,
        debug: &mut crate::Debug,
    ) -> model::UnitAction {
        fn distance_sqr(a: &model::Vec2F64, b: &model::Vec2F64) -> f64 {
            (a.x - b.x).powi(2) + (a.y - b.y).powi(2)
        }
        let nearest_enemy = game
            .units
            .iter()
            .filter(|other| other.player_id != unit.player_id)
            .min_by(|a, b| {
                std::cmp::PartialOrd::partial_cmp(
                    &distance_sqr(&a.position, &unit.position),
                    &distance_sqr(&b.position, &unit.position),
                )
                .unwrap()
            });
        let nearest_weapon = game
            .loot_boxes
            .iter()
            .filter(|loot| {
                if let model::Item::Weapon { .. } = loot.item {
                    true
                } else {
                    false
                }
            })
            .min_by(|a, b| {
                std::cmp::PartialOrd::partial_cmp(
                    &distance_sqr(&a.position, &unit.position),
                    &distance_sqr(&b.position, &unit.position),
                )
                .unwrap()
            });
        let mut target_pos = unit.position.clone();
        if let (&None, Some(weapon)) = (&unit.weapon, nearest_weapon) {
            target_pos = weapon.position.clone();
        } else if let Some(enemy) = nearest_enemy {
            target_pos = enemy.position.clone();
        }
        debug.draw(model::CustomData::Log {
            text: format!("Target pos: {:?}", target_pos),
        });
        let mut aim = model::Vec2F64 { x: 0.0, y: 0.0 };
        if let Some(enemy) = nearest_enemy {
            aim = model::Vec2F64 {
                x: enemy.position.x - unit.position.x,
                y: enemy.position.y - unit.position.y,
            };
        }
        let mut jump = target_pos.y > unit.position.y;
        if target_pos.x > unit.position.x
            && game.level.tiles[(unit.position.x + 1.0) as usize][(unit.position.y) as usize]
                == model::Tile::Wall
        {
            jump = true
        }
        if target_pos.x < unit.position.x
            && game.level.tiles[(unit.position.x - 1.0) as usize][(unit.position.y) as usize]
                == model::Tile::Wall
        {
            jump = true
        }
        model::UnitAction {
            velocity: target_pos.x - unit.position.x,
            jump,
            jump_down: target_pos.y < unit.position.y,
            aim,
            shoot: true,
            swap_weapon: false,
            plant_mine: false,
        }
    }
}
