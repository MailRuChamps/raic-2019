use super::*;

#[derive(geng::Assets)]
pub struct Assets {
    #[asset(path = "idle.png")]
    idle: ugli::Texture,
    #[asset(path = "preparing.png")]
    preparing: ugli::Texture,
    #[asset(path = "triggered.png")]
    triggered: ugli::Texture,
}

impl Assets {
    fn setup(&mut self) {
        self.idle.set_filter(ugli::Filter::Nearest);
        self.preparing.set_filter(ugli::Filter::Nearest);
        self.triggered.set_filter(ugli::Filter::Nearest);
    }
}

pub struct FancyMineRenderer {
    sprite_renderer: Rc<SpriteRenderer>,
    assets: Assets,
}

impl FancyMineRenderer {
    pub fn new(sprite_renderer: &Rc<SpriteRenderer>, mut assets: Assets) -> Self {
        assets.setup();
        Self {
            sprite_renderer: sprite_renderer.clone(),
            assets,
        }
    }
    pub fn draw(
        &mut self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        game: &model::Game,
    ) {
        let camera_uniforms = camera.uniforms(framebuffer);
        for mine in &game.mines {
            let texture = match mine.state {
                model::MineState::Preparing { .. } => &self.assets.preparing,
                model::MineState::Idle => &self.assets.idle,
                model::MineState::Triggered { .. } | model::MineState::Exploded => {
                    &self.assets.triggered
                }
            };
            self.sprite_renderer.draw(
                framebuffer,
                &camera_uniforms,
                texture,
                vec2(0.5, 0.0),
                0.04,
                mine.position.map(|x| x.raw() as f32),
                0.0,
                false,
                Color::WHITE,
            );
        }
    }
}
