use crate::*;

pub struct QuickstartPlayer {}

impl QuickstartPlayer {
    pub fn new() -> Self {
        Self {}
    }
}

impl codegame::Player<model::Game> for QuickstartPlayer {
    fn get_action(
        &mut self,
        view: &model::PlayerView,
        _custom_data_handler: Option<&dyn Fn(model::CustomData)>,
    ) -> Result<model::Action, codegame::PlayerError> {
        Ok(model::ActionWrapper::V1 {
            actions: view
                .game
                .units
                .iter()
                .filter(|unit| unit.player_id == view.my_id)
                .map(|unit| {
                    let nearest_enemy = view
                        .game
                        .units
                        .iter()
                        .filter(|other| other.player_id != unit.player_id)
                        .min_by(|a, b| {
                            std::cmp::PartialOrd::partial_cmp(
                                &(a.position - unit.position).len(),
                                &(b.position - unit.position).len(),
                            )
                            .unwrap()
                        });
                    let nearest_weapon = view
                        .game
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
                                &(a.position - unit.position).len(),
                                &(b.position - unit.position).len(),
                            )
                            .unwrap()
                        });
                    let target_pos = if unit.weapon.is_none() {
                        nearest_weapon.map(|loot| loot.position)
                    } else {
                        nearest_enemy.map(|enemy| enemy.position)
                    }
                    .unwrap_or(unit.position);
                    let direction = if target_pos.x > unit.position.x {
                        1
                    } else {
                        -1
                    };
                    let jump = target_pos.y > unit.position.y
                        || view.game.level.get(
                            (unit.position.x.raw() as i32 + direction) as usize,
                            unit.position.y.raw() as usize,
                        ) == Some(&model::Tile::Wall);
                    (
                        unit.id,
                        model::UnitAction {
                            velocity: target_pos.x - unit.position.x,
                            jump,
                            jump_down: !jump,
                            aim: nearest_enemy
                                .map(|enemy| enemy.position - unit.position)
                                .unwrap_or(vec2(r64(0.0), r64(0.0))),
                            shoot: true,
                            reload: false,
                            swap_weapon: false,
                            plant_mine: false,
                        },
                    )
                })
                .collect(),
        })
    }
}

#[cfg(feature = "rendering")]
mod config {
    use super::*;

    #[derive(Clone)]
    pub struct Config {
        geng: Rc<Geng>,
        theme: Rc<ui::Theme>,
    }

    impl codegame::PlayerConfig<model::Game> for Config {
        fn name(&self) -> &str {
            "quickstart"
        }
        fn ui<'a>(&'a mut self) -> Box<dyn ui::Widget + 'a> {
            use ui::*;
            let ui = ui::text(
                translate("Quickstart strategy"),
                &self.theme.font,
                16.0,
                Color::GRAY,
            )
            .align(vec2(0.5, 1.0));
            Box::new(ui)
        }
        fn ready(&mut self) -> bool {
            true
        }
        fn get(&mut self) -> Box<dyn codegame::Player<model::Game>> {
            Box::new(QuickstartPlayer::new())
        }
        fn to_options(&self) -> PlayerOptions {
            PlayerOptions::Quickstart
        }
    }

    impl Config {
        pub fn new(geng: &Rc<Geng>, theme: &Rc<geng::ui::Theme>) -> Self {
            Self {
                geng: geng.clone(),
                theme: theme.clone(),
            }
        }
        pub fn constructor(
            geng: &Rc<Geng>,
            theme: &Rc<ui::Theme>,
        ) -> Box<dyn Fn() -> Box<dyn codegame::PlayerConfig<model::Game>>> {
            let geng = geng.clone();
            let theme = theme.clone();
            Box::new(move || Box::new(Self::new(&geng, &theme)))
        }
    }
}
#[cfg(feature = "rendering")]
pub use config::Config;
