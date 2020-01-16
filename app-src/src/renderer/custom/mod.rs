use super::*;

use model::ColoredVertex as Vertex;

pub struct CustomRenderer {
    geng: Rc<Geng>,
    program: ugli::Program,
    geometry: ugli::VertexBuffer<Vertex>,
}

impl CustomRenderer {
    pub fn new(geng: &Rc<Geng>) -> Self {
        Self {
            geng: geng.clone(),
            program: geng
                .shader_lib()
                .compile(include_str!("program.glsl"))
                .unwrap(),
            geometry: ugli::VertexBuffer::new_dynamic(geng.ugli(), Vec::new()),
        }
    }
    pub fn draw(
        &mut self,
        framebuffer: &mut ugli::Framebuffer,
        camera_uniforms: impl ugli::Uniforms,
        geometry: &[Vertex],
    ) {
        self.geometry.clear();
        self.geometry.extend_from_slice(geometry);
        ugli::draw(
            framebuffer,
            &self.program,
            ugli::DrawMode::TriangleFan,
            &self.geometry,
            camera_uniforms,
            ugli::DrawParameters {
                blend_mode: Some(default()),
                ..default()
            },
        );
    }

    pub fn draw_all(
        &mut self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        custom_data: &HashMap<usize, Vec<model::CustomData>>,
    ) {
        let camera_uniforms = camera.uniforms(framebuffer);
        let mut y = framebuffer.size().y as f32 - 100.0;
        for player_custom_data in custom_data.values() {
            for data in player_custom_data {
                match *data {
                    model::CustomData::Log { ref text } => {
                        self.geng.default_font().draw(
                            framebuffer,
                            text,
                            vec2(10.0, y),
                            20.0,
                            Color::WHITE,
                        );
                        y += 20.0;
                    }
                    model::CustomData::PlacedText {
                        ref text,
                        pos,
                        size,
                        alignment,
                        color,
                    } => {
                        if size > 1.0 {
                            let pos = camera
                                .world_to_screen(framebuffer, pos.map(|x| r64(x as f64)))
                                .map(|x| x.raw() as f32);
                            let align = match alignment {
                                model::TextAlignment::Left => 0.0,
                                model::TextAlignment::Center => 0.5,
                                model::TextAlignment::Right => 1.0,
                            };
                            self.geng.default_font().draw_aligned(
                                framebuffer,
                                text,
                                pos,
                                align,
                                size,
                                color,
                            );
                        }
                    }
                    model::CustomData::Rect { pos, size, color } => {
                        self.draw(
                            framebuffer,
                            &camera_uniforms,
                            &[
                                Vertex {
                                    position: pos,
                                    color,
                                },
                                Vertex {
                                    position: pos + vec2(size.x, 0.0),
                                    color,
                                },
                                Vertex {
                                    position: pos + size,
                                    color,
                                },
                                Vertex {
                                    position: pos + vec2(0.0, size.y),
                                    color,
                                },
                            ],
                        );
                    }
                    model::CustomData::Line {
                        p1,
                        p2,
                        width,
                        color,
                    } => {
                        let n = (p2 - p1).rotate_90().normalize();
                        let width = width / 2.0;
                        self.draw(
                            framebuffer,
                            &camera_uniforms,
                            &[
                                Vertex {
                                    position: p1 + n * width,
                                    color,
                                },
                                Vertex {
                                    position: p2 + n * width,
                                    color,
                                },
                                Vertex {
                                    position: p2 - n * width,
                                    color,
                                },
                                Vertex {
                                    position: p1 - n * width,
                                    color,
                                },
                            ],
                        );
                    }
                    model::CustomData::Polygon { ref vertices } => {
                        self.draw(framebuffer, &camera_uniforms, vertices);
                    }
                }
            }
        }
    }
}
