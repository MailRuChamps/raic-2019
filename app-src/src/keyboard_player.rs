use crate::*;

#[derive(Clone)]
pub struct Input {
    pub pressed_keys: HashSet<geng::Key>,
    pub pressed_buttons: HashSet<geng::MouseButton>,
    pub mouse_pos: Vec2<R64>,
}

pub struct KeyboardPlayer {
    pub input: Arc<Mutex<Input>>,
    pub left: geng::Key,
    pub right: geng::Key,
    pub jump: geng::Key,
    pub jump_down: geng::Key,
    pub shoot: geng::MouseButton,
    pub reload: geng::Key,
    pub swap_weapon: geng::Key,
    pub last_swap_weapon: bool,
    pub plant_mine: geng::Key,
    pub last_plant_mine: bool,
}

impl KeyboardPlayer {
    pub fn new(input: &Arc<Mutex<Input>>) -> Self {
        Self {
            input: input.clone(),
            left: geng::Key::A,
            right: geng::Key::D,
            jump: geng::Key::W,
            jump_down: geng::Key::S,
            shoot: geng::MouseButton::Left,
            reload: geng::Key::R,
            swap_weapon: geng::Key::E,
            last_swap_weapon: false,
            plant_mine: geng::Key::Q,
            last_plant_mine: false,
        }
    }
}

#[derive(Clone)]
pub struct Config {
    input: Arc<Mutex<Input>>,
    geng: Rc<Geng>,
    theme: Rc<geng::ui::Theme>,
}

impl codegame::PlayerConfig<model::Game> for Config {
    fn name(&self) -> &str {
        translate("keyboard")
    }
    fn ui<'a>(&'a mut self) -> Box<dyn ui::Widget + 'a> {
        use ui::*;
        let ui = ui::text(
            translate("Keyboard player"),
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
        Box::new(KeyboardPlayer::new(&self.input))
    }
    fn to_options(&self) -> PlayerOptions {
        PlayerOptions::Keyboard
    }
}

impl Config {
    pub fn new(geng: &Rc<Geng>, theme: &Rc<geng::ui::Theme>, input: &Arc<Mutex<Input>>) -> Self {
        Self {
            geng: geng.clone(),
            theme: theme.clone(),
            input: input.clone(),
        }
    }
    pub fn constructor(
        geng: &Rc<Geng>,
        theme: &Rc<ui::Theme>,
        input: &Arc<Mutex<Input>>,
    ) -> Box<dyn Fn() -> Box<dyn codegame::PlayerConfig<model::Game>>> {
        let geng = geng.clone();
        let theme = theme.clone();
        let input = input.clone();
        Box::new(move || Box::new(Self::new(&geng, &theme, &input)))
    }
}

impl codegame::Player<model::Game> for KeyboardPlayer {
    fn get_action(
        &mut self,
        view: &model::PlayerView,
        custom_data_handler: Option<&dyn Fn(model::CustomData)>,
    ) -> Result<model::Action, codegame::PlayerError> {
        Ok({
            let input = self.input.lock().unwrap().clone();
            if let Some(handler) = custom_data_handler {
                if let Some(unit) = view
                    .game
                    .units
                    .iter()
                    .find(|unit| unit.player_id == view.my_id)
                {
                    if let Some(weapon) = &unit.weapon {
                        let width = weapon.params.bullet.size;
                        let gap_size = width + r64(0.5) * weapon.spread / weapon.params.max_spread;
                        let radius = gap_size / r64(2.0) + r64(0.5);
                        let color = Color::rgba(0.0, 1.0, 0.0, 0.5);
                        handler(model::CustomData::Rect {
                            pos: (input.mouse_pos + vec2(-width / r64(2.0), gap_size / r64(2.0)))
                                .map(|x| x.raw() as f32),
                            size: vec2(width, radius).map(|x| x.raw() as f32),
                            color,
                        });
                        handler(model::CustomData::Rect {
                            pos: (input.mouse_pos + vec2(-width / r64(2.0), -gap_size / r64(2.0)))
                                .map(|x| x.raw() as f32),
                            size: vec2(width, -radius).map(|x| x.raw() as f32),
                            color,
                        });
                        handler(model::CustomData::Rect {
                            pos: (input.mouse_pos + vec2(gap_size / r64(2.0), -width / r64(2.0)))
                                .map(|x| x.raw() as f32),
                            size: vec2(radius, width).map(|x| x.raw() as f32),
                            color,
                        });
                        handler(model::CustomData::Rect {
                            pos: (input.mouse_pos + vec2(-gap_size / r64(2.0), -width / r64(2.0)))
                                .map(|x| x.raw() as f32),
                            size: vec2(-radius, width).map(|x| x.raw() as f32),
                            color,
                        });
                    }
                }
            }
            let mut velocity = r64(0.0);
            if input.pressed_keys.contains(&self.left) {
                velocity -= r64(1.0);
            }
            if input.pressed_keys.contains(&self.right) {
                velocity += r64(1.0);
            }
            let jump = input.pressed_keys.contains(&self.jump);
            let jump_down = input.pressed_keys.contains(&self.jump_down);

            let mut unit_actions = HashMap::new();
            for unit in view
                .game
                .units
                .iter()
                .filter(|unit| unit.player_id == view.my_id)
            {
                let unit_pos = unit.position + vec2(r64(0.0), unit.size.y / r64(2.0));
                unit_actions.insert(
                    unit.id,
                    model::UnitAction {
                        velocity: velocity * view.game.properties.unit_max_horizontal_speed,
                        jump,
                        jump_down,
                        aim: input.mouse_pos - unit_pos,
                        shoot: input.pressed_buttons.contains(&self.shoot),
                        swap_weapon: if input.pressed_keys.contains(&self.swap_weapon) {
                            if !self.last_swap_weapon {
                                self.last_swap_weapon = true;
                                true
                            } else {
                                false
                            }
                        } else {
                            self.last_swap_weapon = false;
                            false
                        },
                        reload: input.pressed_keys.contains(&self.reload),
                        plant_mine: if input.pressed_keys.contains(&self.plant_mine) {
                            if !self.last_plant_mine {
                                self.last_plant_mine = true;
                                true
                            } else {
                                false
                            }
                        } else {
                            self.last_plant_mine = false;
                            false
                        },
                    },
                );
            }
            model::ActionWrapper::V1 {
                actions: unit_actions,
            }
        })
    }
}
