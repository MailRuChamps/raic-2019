use super::*;

pub struct Camera {
    pub center: Vec2<R64>,
    pub fov: R64,
}

impl Camera {
    pub fn new() -> Self {
        Self {
            center: vec2(r64(0.0), r64(0.0)),
            fov: r64(100.0),
        }
    }
    fn view_matrix(&self) -> Mat4<R64> {
        Mat4::scale_uniform(r64(1.0) / self.fov) * Mat4::translate(-self.center.extend(r64(0.0)))
    }
    fn projection_matrix(&self, framebuffer: &ugli::Framebuffer) -> Mat4<R64> {
        let framebuffer_size = framebuffer.size().map(|x| r64(x as _));
        Mat4::scale(vec3(framebuffer_size.y / framebuffer_size.x, r64(1.0), r64(1.0)) * r64(2.0))
    }
    pub fn uniforms(&self, framebuffer: &ugli::Framebuffer) -> impl ugli::Uniforms {
        ugli::uniforms! {
            u_projection_matrix: self.projection_matrix(framebuffer).map(|x| x.raw() as f32),
            u_view_matrix: self.view_matrix().map(|x| x.raw() as f32),
        }
    }
    pub fn screen_to_world(&self, framebuffer: &ugli::Framebuffer, pos: Vec2<R64>) -> Vec2<R64> {
        let framebuffer_size = framebuffer.size().map(|x| r64(x as _));
        let pos = vec2(
            pos.x / framebuffer_size.x * r64(2.0) - r64(1.0),
            pos.y / framebuffer_size.y * r64(2.0) - r64(1.0),
        );
        let pos = (self.projection_matrix(framebuffer) * self.view_matrix()).inverse()
            * pos.extend(r64(0.0)).extend(r64(1.0));
        pos.xy()
    }
    pub fn world_to_screen(&self, framebuffer: &ugli::Framebuffer, pos: Vec2<R64>) -> Vec2<R64> {
        let framebuffer_size = framebuffer.size().map(|x| r64(x as _));
        let pos = (self.projection_matrix(framebuffer) * self.view_matrix())
            * pos.extend(r64(0.0)).extend(r64(1.0));
        vec2(
            (pos.x + r64(1.0)) / r64(2.0) * framebuffer_size.x,
            (pos.y + r64(1.0)) / r64(2.0) * framebuffer_size.y,
        )
    }
}
