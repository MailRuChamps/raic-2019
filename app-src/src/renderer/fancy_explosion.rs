use super::*;

#[derive(geng::Assets)]
pub struct Assets {
    #[asset(path = "explosion.png")]
    pub explosion: ugli::Texture,
}

impl Assets {
    fn setup(&mut self) {
        self.explosion.set_filter(ugli::Filter::Nearest);
    }
}

pub struct FancyExplosionRenderer {
    sprite_renderer: Rc<SpriteRenderer>,
    assets: Assets,
}

impl FancyExplosionRenderer {
    const DURATION: f32 = 0.2;
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
        explosions: &[Explosion],
        game: &model::Game,
    ) {
        let camera_uniforms = camera.uniforms(framebuffer);
        let texture = &self.assets.explosion;
        let texture_size = texture.size().map(|x| x as f32);
        for explosion in explosions {
            let size = 2.0 * explosion.params.radius.raw() as f32 / texture_size.y;
            let t = (game.current_tick - explosion.tick) as f32
                / game.properties.ticks_per_second.raw() as f32
                / Self::DURATION;
            if t > 1.0 {
                continue;
            }
            let size = size * t;
            self.sprite_renderer.draw(
                framebuffer,
                &camera_uniforms,
                texture,
                vec2(0.5, 0.5),
                size,
                explosion.position.map(|x| x.raw() as f32),
                0.0,
                false,
                Color::rgba(1.0, 1.0, 1.0, 1.0 - t),
            );
        }
    }
}
