use crate::*;

struct Data {
    close: bool,
    change_tile: bool,
    tile_button: ui::TextButton,
    save_button: ui::TextButton,
}

impl Data {
    fn ui<'a>(&'a mut self) -> impl ui::Widget + 'a {
        use ui::*;
        let change_tile = &mut self.change_tile;
        let close = &mut self.close;
        stack![
            self.tile_button
                .ui(Box::new(move || {
                    *change_tile = true;
                }))
                .align(vec2(0.5, 0.0))
                .uniform_padding(8.0),
            self.save_button
                .ui(Box::new(move || {
                    *close = true;
                }))
                .align(vec2(1.0, 0.0))
                .uniform_padding(8.0),
        ]
    }
}

pub struct LevelEditor {
    geng: Rc<Geng>,
    simple_renderer: Rc<SimpleRenderer>,
    camera: Camera,
    level: model::Level,
    spawn_points: HashSet<Vec2<usize>>,
    level_renderer: LevelRenderer,
    level_options: Rc<RefCell<Option<model::LevelOptions>>>,
    mouse_pos: Vec2<R64>,
    ui_controller: ui::Controller,
    data: Data,
    tile_options: Vec<model::Tile>,
    current_tile: usize,
}

impl LevelEditor {
    pub fn new(
        geng: &Rc<Geng>,
        simple_renderer: &Rc<SimpleRenderer>,
        theme: &Rc<ui::Theme>,
        level_options: &Rc<RefCell<Option<model::LevelOptions>>>,
        size: Vec2<usize>,
    ) -> Self {
        Self {
            geng: geng.clone(),
            simple_renderer: simple_renderer.clone(),
            camera: Camera::new(),
            level: level_options.borrow().as_ref().map_or_else(
                || model::Level {
                    tiles: vec![vec![model::Tile::Empty; size.y]; size.x],
                },
                |options| options.clone().create().0,
            ),
            spawn_points: HashSet::new(),
            level_renderer: LevelRenderer::new(geng, simple_renderer),
            mouse_pos: vec2(r64(0.0), r64(0.0)),
            ui_controller: ui::Controller::new(),
            data: Data {
                close: false,
                change_tile: false,
                tile_button: ui::TextButton::new(geng, theme, String::new(), 32.0),
                save_button: ui::TextButton::new(geng, theme, translate("save").to_owned(), 32.0),
            },
            tile_options: vec![
                model::Tile::Empty,
                model::Tile::Wall,
                model::Tile::Platform,
                model::Tile::Ladder,
                model::Tile::JumpPad,
            ],
            current_tile: 0,
            level_options: level_options.clone(),
        }
    }
}

impl geng::State for LevelEditor {
    fn update(&mut self, delta_time: f64) {
        if self
            .geng
            .window()
            .is_button_pressed(geng::MouseButton::Left)
        {
            if self.mouse_pos.x >= r64(0.0) && self.mouse_pos.y >= r64(0.0) {
                let pos = self.mouse_pos.map(|x| x.raw() as usize);
                if let Some(tile) = self
                    .level
                    .tiles
                    .get_mut(pos.x)
                    .and_then(|col| col.get_mut(pos.y))
                {
                    *tile = self.tile_options[self.current_tile].clone();
                }
            }
        }
        self.ui_controller.update(self.data.ui(), delta_time);
    }
    fn draw(&mut self, framebuffer: &mut ugli::Framebuffer) {
        ugli::clear(framebuffer, Some(Color::BLACK), None);
        self.camera.center = self.level.size().map(|x| r64(x as _)) / r64(2.0);
        self.camera.fov = r64(self.level.size().y as f64 + 5.0);
        self.mouse_pos = self
            .camera
            .screen_to_world(framebuffer, self.geng.window().mouse_pos().map(|x| r64(x)));
        self.data.tile_button.text = format!("{:?}", self.tile_options[self.current_tile]);
        self.level_renderer
            .draw(framebuffer, &self.camera, &self.level, None, None);
        for pos in &self.spawn_points {
            self.simple_renderer.frame(
                framebuffer,
                &self.camera,
                AABB::pos_size(pos.map(|x| r64(x as _)), vec2(r64(1.0), r64(2.0))),
                r64(0.1),
                Color::rgba(1.0, 0.5, 0.5, 0.7),
            );
        }
        if self.mouse_pos.x >= r64(0.0)
            && self.mouse_pos.y >= r64(0.0)
            && self
                .level
                .tiles
                .get(self.mouse_pos.x.raw() as usize)
                .and_then(|col| col.get(self.mouse_pos.y.raw() as usize))
                .is_some()
        {
            self.simple_renderer.frame(
                framebuffer,
                &self.camera,
                AABB::pos_size(self.mouse_pos.map(|x| x.floor()), vec2(r64(1.0), r64(1.0))),
                r64(0.1),
                Color::rgba(0.5, 0.5, 1.0, 0.7),
            );
        }
        self.ui_controller.draw(self.data.ui(), framebuffer);
    }
    fn handle_event(&mut self, event: geng::Event) {
        match event {
            geng::Event::KeyDown { key } => match key {
                geng::Key::Escape => {
                    self.data.close = true;
                }
                #[cfg(not(any(target_arch = "asmjs", target_arch = "wasm32")))]
                geng::Key::S if self.geng.window().is_key_pressed(geng::Key::LCtrl) => {
                    save_file(translate("save level"), "level.txt", |writer| {
                        self.level.save(writer, &self.spawn_points)
                    })
                    .expect("Failed to save level");
                }
                _ => {}
            },
            geng::Event::MouseDown { button, .. } => match button {
                geng::MouseButton::Right => {
                    if self.mouse_pos.x >= r64(0.0) && self.mouse_pos.y >= r64(0.0) {
                        let pos = self.mouse_pos.map(|x| x.raw() as usize);
                        if pos.x < self.level.size().x && pos.y < self.level.size().y {
                            if self.spawn_points.contains(&pos) {
                                self.spawn_points.remove(&pos);
                            } else {
                                self.spawn_points.insert(pos);
                            }
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }
        self.ui_controller.handle_event(self.data.ui(), event);
        if self.data.change_tile {
            self.data.change_tile = false;
            self.current_tile = (self.current_tile + 1) % self.tile_options.len();
        }
    }
    fn transition(&mut self) -> Option<geng::Transition> {
        if self.data.close {
            *self.level_options.borrow_mut() = Some(model::LevelOptions::Ready {
                level: self.level.clone(),
                spawn_points: self
                    .spawn_points
                    .iter()
                    .map(|pos| vec2(r64(pos.x as f64 + 0.5), r64(pos.y as _)))
                    .collect(),
            });
            Some(geng::Transition::Pop)
        } else {
            None
        }
    }
}
