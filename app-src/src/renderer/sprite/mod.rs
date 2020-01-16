use super::*;

#[derive(ugli::Vertex)]
pub struct Vertex {
    pub a_pos: Vec2<f32>,
}

pub struct SpriteRenderer {
    program: ugli::Program,
    quad: ugli::VertexBuffer<Vertex>,
}

impl SpriteRenderer {
    pub fn new(geng: &Rc<Geng>) -> Self {
        Self {
            program: geng
                .shader_lib()
                .compile(include_str!("program.glsl"))
                .unwrap(),
            quad: ugli::VertexBuffer::new_static(
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
        }
    }
    pub fn draw(
        &self,
        framebuffer: &mut ugli::Framebuffer,
        camera_uniforms: impl ugli::Uniforms,
        texture: &ugli::Texture,
        origin: Vec2<f32>,
        scale: f32,
        pos: Vec2<f32>,
        rotation: f32,
        flip: bool,
        color: Color<f32>,
    ) {
        let texture_size = texture.size().map(|x| x as f32) * scale;
        let origin = if flip {
            vec2(1.0 - origin.x, origin.y)
        } else {
            origin
        };
        ugli::draw(
            framebuffer,
            &self.program,
            ugli::DrawMode::TriangleFan,
            &self.quad,
            (
                camera_uniforms,
                ugli::uniforms! {
                    u_texture: texture,
                    u_rotation: rotation,
                    u_pos: pos,
                    u_flip: if flip { 0.0 } else { 1.0 },
                    u_texture_size: texture_size,
                    u_origin: origin,
                    u_color: color,
                },
            ),
            ugli::DrawParameters {
                blend_mode: Some(default()),
                ..default()
            },
        );
    }
}
