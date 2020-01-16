use super::*;

pub struct LevelRenderer {
    simple_renderer: Rc<SimpleRenderer>,
    quads: ugli::VertexBuffer<simple::Instance>,
    last_drawn_level: Option<model::Level>,
}

impl LevelRenderer {
    const JUMP_PAD_ANIMATION_TIME: f32 = 0.2;
    pub fn new(geng: &Rc<Geng>, simple_renderer: &Rc<SimpleRenderer>) -> Self {
        Self {
            simple_renderer: simple_renderer.clone(),
            quads: ugli::VertexBuffer::new_static(geng.ugli(), vec![]),
            last_drawn_level: None,
        }
    }
    pub fn draw(
        &mut self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        level: &model::Level,
        game: Option<&model::Game>,
        extra_data: Option<&RendererExtraData>,
    ) {
        if true {
            // TODO: caching breaks jump pad animation
            // self.last_drawn_map.as_ref() != Some(&game.map) {
            let quads: &mut Vec<simple::Instance> = &mut self.quads;
            quads.clear();
            for (pos, tile) in level {
                match tile {
                    model::Tile::Empty => {}
                    model::Tile::Wall => {
                        quads.push(simple::Instance {
                            i_pos: pos.map(|x| x as f32),
                            i_size: vec2(1.0, 1.0),
                            i_color: Color::GRAY,
                        });
                    }
                    model::Tile::Platform => {
                        quads.push(simple::Instance {
                            i_pos: pos.map(|x| x as f32) + vec2(0.0, 0.9),
                            i_size: vec2(1.0, 0.1),
                            i_color: Color::GRAY,
                        });
                    }
                    model::Tile::Ladder => {
                        quads.push(simple::Instance {
                            i_pos: pos.map(|x| x as f32),
                            i_size: vec2(0.1, 1.0),
                            i_color: Color::GRAY,
                        });
                        quads.push(simple::Instance {
                            i_pos: pos.map(|x| x as f32) + vec2(1.0, 0.0),
                            i_size: vec2(-0.1, 1.0),
                            i_color: Color::GRAY,
                        });
                        quads.push(simple::Instance {
                            i_pos: pos.map(|x| x as f32) + vec2(0.0, 0.4),
                            i_size: vec2(1.0, 0.1),
                            i_color: Color::GRAY,
                        });
                        quads.push(simple::Instance {
                            i_pos: pos.map(|x| x as f32) + vec2(0.0, 0.9),
                            i_size: vec2(1.0, 0.1),
                            i_color: Color::GRAY,
                        });
                    }
                    model::Tile::JumpPad => {
                        let height = if let Some(game) = game {
                            if let Some(last_used_tick) = extra_data.and_then(|extra_data| {
                                extra_data.level_last_used_tick.get(&pos).copied()
                            }) {
                                let t = ((game.current_tick as f32 - 1.0 - last_used_tick as f32)
                                    / game.properties.ticks_per_second.raw() as f32
                                    / Self::JUMP_PAD_ANIMATION_TIME)
                                    .min(1.0);
                                let t = if t < 0.0 { 1.0 } else { t };
                                1.0 - t * 0.5
                            } else {
                                0.5
                            }
                        } else {
                            0.5
                        };

                        quads.push(simple::Instance {
                            i_pos: pos.map(|x| x as f32) + vec2(0.3, 0.0),
                            i_size: vec2(0.4, height - 0.1),
                            i_color: Color::GRAY,
                        });
                        quads.push(simple::Instance {
                            i_pos: pos.map(|x| x as f32) + vec2(0.0, height - 0.2),
                            i_size: vec2(1.0, 0.2),
                            i_color: Color::YELLOW,
                        });
                    }
                }
            }
            self.last_drawn_level = Some(level.clone());
        }
        self.simple_renderer.frame(
            framebuffer,
            camera,
            AABB::pos_size(vec2(r64(0.0), r64(0.0)), level.size().map(|x| r64(x as _))),
            r64(0.1),
            Color::GRAY,
        );
        self.simple_renderer.quads(framebuffer, camera, &self.quads);
    }
}
