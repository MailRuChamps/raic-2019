use super::*;

pub struct BulletRenderer {
    simple_renderer: Rc<SimpleRenderer>,
}

impl BulletRenderer {
    pub fn new(simple_renderer: &Rc<SimpleRenderer>) -> Self {
        Self {
            simple_renderer: simple_renderer.clone(),
        }
    }
    pub fn draw(
        &mut self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        game: &model::Game,
    ) {
        for bullet in &game.bullets {
            self.simple_renderer
                .quad(framebuffer, camera, bullet.rect(), Color::WHITE);
        }
    }
}
