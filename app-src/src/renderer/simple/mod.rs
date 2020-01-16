use super::*;

#[derive(ugli::Vertex)]
pub struct Vertex {
    pub a_pos: Vec2<f32>,
}

#[derive(ugli::Vertex)]
pub struct Instance {
    pub i_pos: Vec2<f32>,
    pub i_size: Vec2<f32>,
    pub i_color: Color<f32>,
}

pub struct SimpleRenderer {
    quad_geometry: ugli::VertexBuffer<Vertex>,
    instances: RefCell<ugli::VertexBuffer<Instance>>,
    program: ugli::Program,
    white_texture: ugli::Texture,
}

impl SimpleRenderer {
    pub fn new(geng: &Rc<Geng>) -> Self {
        Self {
            quad_geometry: ugli::VertexBuffer::new_static(
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
            instances: RefCell::new(ugli::VertexBuffer::new_dynamic(geng.ugli(), vec![])),
            program: geng
                .shader_lib()
                .compile(include_str!("program.glsl"))
                .unwrap(),
            white_texture: ugli::Texture::new_with(geng.ugli(), vec2(1, 1), |_| Color::WHITE),
        }
    }
    pub fn textured_quad(
        &self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        rect: AABB<R64>,
        texture: &ugli::Texture,
    ) {
        let rect = rect.map(|x| x.raw() as f32);
        let mut instances = self.instances.borrow_mut();
        instances.clear();
        instances.push(Instance {
            i_pos: rect.bottom_left(),
            i_size: rect.size(),
            i_color: Color::WHITE,
        });
        self.draw(
            framebuffer,
            camera,
            &self.quad_geometry,
            &instances,
            Some(texture),
        );
    }
    pub fn quad(
        &self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        rect: AABB<R64>,
        color: Color<f32>,
    ) {
        let rect = rect.map(|x| x.raw() as f32);
        let mut instances = self.instances.borrow_mut();
        instances.clear();
        instances.push(Instance {
            i_pos: rect.bottom_left(),
            i_size: rect.size(),
            i_color: color,
        });
        self.quads(framebuffer, camera, &instances);
    }
    pub fn frame(
        &self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        rect: AABB<R64>,
        width: R64,
        color: Color<f32>,
    ) {
        self.quad(
            framebuffer,
            camera,
            AABB::pos_size(rect.bottom_left(), vec2(-width, rect.height())),
            color,
        );
        self.quad(
            framebuffer,
            camera,
            AABB::pos_size(rect.bottom_left(), vec2(rect.width(), -width)),
            color,
        );
        self.quad(
            framebuffer,
            camera,
            AABB::pos_size(rect.top_right(), vec2(-rect.width(), width)),
            color,
        );
        self.quad(
            framebuffer,
            camera,
            AABB::pos_size(rect.top_right(), vec2(width, -rect.height())),
            color,
        );
    }
    pub fn quads(
        &self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        instances: &ugli::VertexBuffer<Instance>,
    ) {
        self.draw(framebuffer, camera, &self.quad_geometry, instances, None);
    }
    pub fn draw(
        &self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        vertices: &ugli::VertexBuffer<Vertex>,
        instances: &ugli::VertexBuffer<Instance>,
        texture: Option<&ugli::Texture>,
    ) {
        let texture = texture.unwrap_or(&self.white_texture);
        let camera_uniforms = camera.uniforms(framebuffer);
        ugli::draw(
            framebuffer,
            &self.program,
            ugli::DrawMode::TriangleFan,
            ugli::instanced(vertices, instances),
            (
                camera_uniforms,
                ugli::uniforms! {
                    u_texture: texture,
                },
            ),
            ugli::DrawParameters {
                blend_mode: Some(default()),
                ..default()
            },
        );
    }
}
