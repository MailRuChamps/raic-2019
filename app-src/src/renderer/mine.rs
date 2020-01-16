use super::*;

pub struct MineRenderer {
    simple_renderer: Rc<SimpleRenderer>,
}

impl MineRenderer {
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
        for mine in &game.mines {
            self.simple_renderer
                .quad(framebuffer, camera, mine.rect(), Color::YELLOW);
            self.simple_renderer.quad(
                framebuffer,
                camera,
                AABB::pos_size(
                    mine.position - vec2(mine.size.x / r64(4.0), r64(0.0)),
                    mine.size / r64(2.0),
                ),
                match mine.state {
                    model::MineState::Preparing { .. } => Color::GREEN,
                    model::MineState::Idle => Color::rgb(1.0, 0.5, 1.0),
                    model::MineState::Triggered { .. } | model::MineState::Exploded => Color::RED,
                },
            );
        }
    }
}
