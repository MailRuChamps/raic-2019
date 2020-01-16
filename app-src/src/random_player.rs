use crate::*;

pub struct RandomPlayer {
    think_ticks: usize,
    next_think: usize,
    last_action: Option<model::Action>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct RandomPlayerOptions {
    pub think_ticks: usize,
}

impl RandomPlayer {
    pub fn new(options: RandomPlayerOptions) -> Self {
        Self {
            think_ticks: options.think_ticks,
            next_think: 0,
            last_action: None,
        }
    }
}

impl codegame::Player<model::Game> for RandomPlayer {
    fn get_action(
        &mut self,
        view: &model::PlayerView,
        _custom_data_handler: Option<&dyn Fn(model::CustomData)>,
    ) -> Result<model::Action, codegame::PlayerError> {
        if self.next_think == 0 || self.last_action.is_none() {
            self.last_action = Some(model::ActionWrapper::V1 {
                actions: view
                    .game
                    .units
                    .iter()
                    .filter(|unit| unit.player_id == view.my_id)
                    .map(|unit| {
                        (
                            unit.id,
                            model::UnitAction {
                                velocity: r64(global_rng().gen_range(
                                    -view.game.properties.unit_max_horizontal_speed.raw(),
                                    view.game.properties.unit_max_horizontal_speed.raw(),
                                )),
                                jump: global_rng().gen(),
                                jump_down: global_rng().gen(),
                                aim: Vec2::rotated(
                                    vec2(r64(1.0), r64(0.0)),
                                    r64(global_rng().gen_range(0.0, 2.0 * std::f64::consts::PI)),
                                ),
                                shoot: global_rng().gen(),
                                reload: global_rng().gen(),
                                swap_weapon: global_rng().gen(),
                                plant_mine: global_rng().gen(),
                            },
                        )
                    })
                    .collect(),
            });
            self.next_think = self.think_ticks;
        }
        self.next_think -= 1;
        Ok(self.last_action.clone().unwrap())
    }
}

#[cfg(feature = "rendering")]
mod config {
    use super::*;

    #[derive(Clone)]
    pub struct Config {
        geng: Rc<Geng>,
        theme: Rc<ui::Theme>,
        slider: ui::Slider,
        options: RandomPlayerOptions,
    }

    impl codegame::PlayerConfig<model::Game> for Config {
        fn name(&self) -> &str {
            translate("random")
        }
        fn ui<'a>(&'a mut self) -> Box<dyn ui::Widget + 'a> {
            use ui::*;
            let think_ticks = &mut self.options.think_ticks;
            let ui = row![
                text(
                    translate("thinking speed"),
                    &self.theme.font,
                    16.0,
                    Color::GRAY
                )
                .padding_right(16.0),
                self.slider
                    .ui(
                        *think_ticks as f64,
                        1.0..=120.0,
                        Box::new(move |new_value| {
                            *think_ticks = new_value as usize;
                        }),
                    )
                    .fixed_size(vec2(50.0, 16.0)),
            ]
            .align(vec2(0.5, 1.0));
            Box::new(ui)
        }
        fn ready(&mut self) -> bool {
            true
        }
        fn get(&mut self) -> Box<dyn codegame::Player<model::Game>> {
            Box::new(RandomPlayer::new(self.options.clone()))
        }
        fn to_options(&self) -> PlayerOptions {
            PlayerOptions::Random(self.options.clone())
        }
    }

    impl Config {
        pub fn new(geng: &Rc<Geng>, theme: &Rc<geng::ui::Theme>) -> Self {
            Self {
                geng: geng.clone(),
                theme: theme.clone(),
                options: RandomPlayerOptions { think_ticks: 60 },
                slider: geng::ui::Slider::new(geng, theme),
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
