use super::*;

#[derive(geng::Assets)]
#[allow(non_snake_case)]
pub struct Assets {
    #[asset(path = "jumper_1.png")]
    jumper_1: ugli::Texture,
    #[asset(path = "jumper_2.png")]
    jumper_2: ugli::Texture,
    #[asset(path = "jumper_3.png")]
    jumper_3: ugli::Texture,
    #[asset(path = "wall_down_L.png")]
    wall_down_L: ugli::Texture,
    #[asset(path = "wall_down_R.png")]
    wall_down_R: ugli::Texture,
    #[asset(path = "wall_down_C.png")]
    wall_down_C: ugli::Texture,
    #[asset(path = "wall_down_solo.png")]
    wall_down_solo: ugli::Texture,
    #[asset(path = "wall_L.png")]
    wall_L: ugli::Texture,
    #[asset(path = "wall_C.png")]
    wall_C: ugli::Texture,
    #[asset(path = "wall_R.png")]
    wall_R: ugli::Texture,
    #[asset(path = "wall_solo.png")]
    wall_solo: ugli::Texture,
    #[asset(path = "window_L.png")]
    window_L: ugli::Texture,
    #[asset(path = "window_R.png")]
    window_R: ugli::Texture,
    #[asset(path = "window_C.png")]
    window_C: ugli::Texture,
    #[asset(path = "window_solo.png")]
    window_solo: ugli::Texture,
    #[asset(path = "railing_L.png")]
    railing_L: ugli::Texture,
    #[asset(path = "railing_C.png")]
    railing_C: ugli::Texture,
    #[asset(path = "railing_R.png")]
    railing_R: ugli::Texture,
    #[asset(path = "platform_C.png")]
    platform_C: ugli::Texture,
    #[asset(path = "platform_L.png")]
    platform_L: ugli::Texture,
    #[asset(path = "platform_R.png")]
    platform_R: ugli::Texture,
    #[asset(path = "platform_solo.png")]
    platform_solo: ugli::Texture,
    #[asset(path = "floor_1.png")]
    floor_1: ugli::Texture,
    #[asset(path = "floor_2.png")]
    floor_2: ugli::Texture,
    #[asset(path = "door.png")]
    door: ugli::Texture,
    #[asset(path = "platform_small_L.png")]
    platform_small_L: ugli::Texture,
    #[asset(path = "platform_small_C.png")]
    platform_small_C: ugli::Texture,
    #[asset(path = "platform_small_R.png")]
    platform_small_R: ugli::Texture,
    #[asset(path = "platform_small_solo.png")]
    platform_small_solo: ugli::Texture,
    #[asset(path = "stairs.png")]
    stairs: ugli::Texture,
}

#[allow(non_snake_case)]
struct TextureAtlas {
    atlas: geng::TextureAtlas,
    jumper_1: usize,
    jumper_2: usize,
    jumper_3: usize,
    wall_down_L: usize,
    wall_down_R: usize,
    wall_down_C: usize,
    wall_down_solo: usize,
    wall_L: usize,
    wall_C: usize,
    wall_R: usize,
    wall_solo: usize,
    window_L: usize,
    window_R: usize,
    window_C: usize,
    window_solo: usize,
    railing_L: usize,
    railing_C: usize,
    railing_R: usize,
    platform_C: usize,
    platform_L: usize,
    platform_R: usize,
    platform_solo: usize,
    floor_1: usize,
    floor_2: usize,
    door: usize,
    platform_small_L: usize,
    platform_small_C: usize,
    platform_small_R: usize,
    platform_small_solo: usize,
    stairs: usize,
}

impl TextureAtlas {
    fn new(ugli: &Rc<Ugli>, assets: Assets) -> Self {
        let mut atlas = geng::TextureAtlas::new(
            ugli,
            &[
                &assets.jumper_1,
                &assets.jumper_2,
                &assets.jumper_3,
                &assets.wall_down_L,
                &assets.wall_down_R,
                &assets.wall_down_C,
                &assets.wall_down_solo,
                &assets.wall_L,
                &assets.wall_C,
                &assets.wall_R,
                &assets.wall_solo,
                &assets.window_L,
                &assets.window_R,
                &assets.window_C,
                &assets.window_solo,
                &assets.railing_L,
                &assets.railing_C,
                &assets.railing_R,
                &assets.platform_C,
                &assets.platform_L,
                &assets.platform_R,
                &assets.platform_solo,
                &assets.floor_1,
                &assets.floor_2,
                &assets.door,
                &assets.platform_small_L,
                &assets.platform_small_C,
                &assets.platform_small_R,
                &assets.platform_small_solo,
                &assets.stairs,
            ],
        );
        atlas.set_filter(ugli::Filter::Nearest);
        Self {
            atlas,
            jumper_1: 0,
            jumper_2: 1,
            jumper_3: 2,
            wall_down_L: 3,
            wall_down_R: 4,
            wall_down_C: 5,
            wall_down_solo: 6,
            wall_L: 7,
            wall_C: 8,
            wall_R: 9,
            wall_solo: 10,
            window_L: 11,
            window_R: 12,
            window_C: 13,
            window_solo: 14,
            railing_L: 15,
            railing_C: 16,
            railing_R: 17,
            platform_C: 18,
            platform_L: 19,
            platform_R: 20,
            platform_solo: 21,
            floor_1: 22,
            floor_2: 23,
            door: 24,
            platform_small_L: 25,
            platform_small_C: 26,
            platform_small_R: 27,
            platform_small_solo: 28,
            stairs: 29,
        }
    }
}

#[derive(ugli::Vertex)]
struct Vertex {
    a_pos: Vec2<f32>,
}

#[derive(ugli::Vertex)]
struct Instance {
    i_pos: Vec2<f32>,
    i_color: Color<f32>,
    i_uv_pos: Vec2<f32>,
    i_uv_size: Vec2<f32>,
}

pub struct FancyLevelRenderer {
    program: ugli::Program,
    texture_atlas: TextureAtlas,
    vertices: ugli::VertexBuffer<Vertex>,
    instances: ugli::VertexBuffer<Instance>,
}

impl FancyLevelRenderer {
    const JUMP_PAD_ANIMATION_TIME: f32 = 0.2;
    pub fn new(geng: &Rc<Geng>, assets: Assets) -> Self {
        Self {
            program: geng
                .shader_lib()
                .compile(include_str!("program.glsl"))
                .unwrap(),
            texture_atlas: TextureAtlas::new(geng.ugli(), assets),
            vertices: ugli::VertexBuffer::new_static(
                geng.ugli(),
                vec![
                    Vertex {
                        a_pos: vec2(0.0, 0.0),
                    },
                    Vertex {
                        a_pos: vec2(1.0, 0.0),
                    },
                    Vertex {
                        a_pos: vec2(1.0, 1.0),
                    },
                    Vertex {
                        a_pos: vec2(0.0, 1.0),
                    },
                ],
            ),
            instances: ugli::VertexBuffer::new_dynamic(geng.ugli(), Vec::new()),
        }
    }
    fn add_quad(&mut self, x: usize, y: usize, texture: usize) {
        self.add_colored_quad(x, y, texture, Color::WHITE);
    }
    fn add_colored_quad(&mut self, x: usize, y: usize, texture: usize, color: Color<f32>) {
        let uv_rect = self.texture_atlas.atlas.uv(texture);
        self.instances.push(Instance {
            i_pos: vec2(x as f32, y as f32),
            i_color: color,
            i_uv_pos: uv_rect.bottom_left(),
            i_uv_size: uv_rect.size(),
        });
    }
    pub fn draw(
        &mut self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        level: &model::Level,
        game: Option<&model::Game>,
        extra_data: Option<&RendererExtraData>,
    ) {
        self.instances.clear();
        let mut has_ceiling = vec![false; level.size().x];
        for y in (0..level.size().y - 1).rev() {
            for x in 1..level.size().x - 1 {
                if level.get(x, y) == Some(&model::Tile::Wall) {
                    has_ceiling[x] = true;
                }
            }
            for x in 0..level.size().x {
                if has_ceiling[x] {
                    let has_left = x > 0 && has_ceiling[x - 1];
                    let has_right = has_ceiling.get(x + 1) == Some(&true);
                    let texture =
                        if y == 1 || y > 0 && level.get(x, y - 1) == Some(&model::Tile::Wall) {
                            match (has_left, has_right) {
                                (true, true) => self.texture_atlas.wall_down_C,
                                (true, false) => self.texture_atlas.wall_down_R,
                                (false, true) => self.texture_atlas.wall_down_L,
                                (false, false) => self.texture_atlas.wall_down_solo,
                            }
                        } else {
                            match (has_left, has_right) {
                                (true, true) => self.texture_atlas.wall_C,
                                (true, false) => self.texture_atlas.wall_R,
                                (false, true) => self.texture_atlas.wall_L,
                                (false, false) => self.texture_atlas.wall_solo,
                            }
                        };
                    self.add_quad(x, y, texture);
                }
            }
        }
        for (pos, tile) in level {
            if pos.y == 0 {
                let texture = if pos.x % 3 == 0 {
                    self.texture_atlas.floor_1
                } else {
                    self.texture_atlas.floor_2
                };
                self.add_quad(pos.x, pos.y, texture);
                continue;
            } else if pos.x == 0 || pos.x + 1 == level.size().x || pos.y + 1 == level.size().y {
                continue;
            }
            match tile {
                model::Tile::Empty => {}
                model::Tile::Wall => {
                    let has_left =
                        pos.x > 0 && level.get(pos.x - 1, pos.y) == Some(&model::Tile::Wall);
                    let has_right = level.get(pos.x + 1, pos.y) == Some(&model::Tile::Wall);
                    let texture = match (has_left, has_right) {
                        (true, true) => self.texture_atlas.platform_C,
                        (true, false) => self.texture_atlas.platform_R,
                        (false, true) => self.texture_atlas.platform_L,
                        (false, false) => self.texture_atlas.platform_solo,
                    };
                    self.add_quad(pos.x, pos.y, texture);
                }
                model::Tile::Platform => {
                    let has_left =
                        pos.x > 0 && level.get(pos.x - 1, pos.y) == Some(&model::Tile::Platform);
                    let has_right = level.get(pos.x + 1, pos.y) == Some(&model::Tile::Platform);
                    let texture = match (has_left, has_right) {
                        (true, true) => self.texture_atlas.platform_small_C,
                        (true, false) => self.texture_atlas.platform_small_R,
                        (false, true) => self.texture_atlas.platform_small_L,
                        (false, false) => self.texture_atlas.platform_small_solo,
                    };
                    self.add_quad(pos.x, pos.y, texture);
                }
                model::Tile::Ladder => {}
                model::Tile::JumpPad => {}
            }
        }
        for y in (2..level.size().y).rev() {
            let mut has_rail = vec![false; level.size().x];
            for x in 0..level.size().x {
                has_rail[x] = level.get(x, y) != Some(&model::Tile::Wall)
                    && y > 0
                    && level.get(x, y - 1) == Some(&model::Tile::Wall);
            }
            for x in 0..level.size().x {
                if has_rail[x] {
                    let has_left = x > 0 && has_rail[x - 1];
                    let has_right = has_rail.get(x + 1) == Some(&true);
                    let texture = match (has_left, has_right) {
                        (true, true) => self.texture_atlas.railing_C,
                        (true, false) => self.texture_atlas.railing_R,
                        (false, true) => self.texture_atlas.railing_L,
                        (false, false) => continue,
                    };
                    self.add_quad(x, y, texture);
                }
            }
        }
        for (pos, tile) in level {
            match tile {
                model::Tile::Ladder => {
                    self.add_quad(pos.x, pos.y, self.texture_atlas.stairs);
                }
                model::Tile::JumpPad => {
                    let mut texture = self.texture_atlas.jumper_1;
                    if let Some(game) = game {
                        if let Some(last_used_tick) = extra_data.and_then(|extra_data| {
                            extra_data.level_last_used_tick.get(&pos).copied()
                        }) {
                            let t = ((game.current_tick as f32 - 1.0 - last_used_tick as f32)
                                / game.properties.ticks_per_second.raw() as f32
                                / Self::JUMP_PAD_ANIMATION_TIME)
                                .min(1.0);
                            let t = if t < 0.0 { 1.0 } else { t };
                            if t < 0.33 {
                                texture = self.texture_atlas.jumper_3;
                            } else if t < 0.66 {
                                texture = self.texture_atlas.jumper_2;
                            }
                        }
                    }
                    self.add_quad(pos.x, pos.y, texture);
                }
                _ => {}
            }
        }
        let camera_uniforms = camera.uniforms(framebuffer);
        ugli::draw(
            framebuffer,
            &self.program,
            ugli::DrawMode::TriangleFan,
            ugli::instanced(&self.vertices, &self.instances),
            (
                camera_uniforms,
                ugli::uniforms! {
                    u_texture: self.texture_atlas.atlas.texture(),
                },
            ),
            ugli::DrawParameters {
                blend_mode: Some(default()),
                ..default()
            },
        );
    }
}
