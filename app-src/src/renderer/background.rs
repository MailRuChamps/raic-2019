use super::*;

#[derive(geng::Assets)]
pub struct Assets {
    #[asset(path = "city.png")]
    city: ugli::Texture,
    #[asset(path = "cloud.png")]
    cloud: ugli::Texture,
}

pub struct BackgroundRenderer {
    geng: Rc<Geng>,
    assets: Assets,
}

impl BackgroundRenderer {
    const CLOUDS_Y: f32 = 160.0 / 576.0;
    const CLOUD_SPEED: f32 = 1.0 / 60.0;
    pub fn new(geng: &Rc<Geng>, mut assets: Assets) -> Self {
        assets.city.set_filter(ugli::Filter::Nearest);
        assets.cloud.set_filter(ugli::Filter::Nearest);
        Self {
            geng: geng.clone(),
            assets,
        }
    }
    pub fn draw(&mut self, framebuffer: &mut ugli::Framebuffer, game: &model::Game) {
        let framebuffer_size = framebuffer.size().map(|x| x as f32);
        self.geng.draw_2d().textured_quad(
            framebuffer,
            AABB::pos_size(vec2(0.0, 0.0), framebuffer_size),
            &self.assets.city,
            Color::WHITE,
        );
        let pos = (game.current_tick as f32 / game.properties.ticks_per_second.raw() as f32
            * Self::CLOUD_SPEED)
            .fract();
        self.geng.draw_2d().textured_quad(
            framebuffer,
            AABB::pos_size(
                vec2(
                    pos * framebuffer_size.x,
                    framebuffer_size.y * (1.0 - Self::CLOUDS_Y),
                ),
                vec2(framebuffer_size.x, framebuffer_size.y * Self::CLOUDS_Y),
            ),
            &self.assets.cloud,
            Color::WHITE,
        );
        self.geng.draw_2d().textured_quad(
            framebuffer,
            AABB::pos_size(
                vec2(
                    pos * framebuffer_size.x - framebuffer_size.x,
                    framebuffer_size.y * (1.0 - Self::CLOUDS_Y),
                ),
                vec2(framebuffer_size.x, framebuffer_size.y * Self::CLOUDS_Y),
            ),
            &self.assets.cloud,
            Color::WHITE,
        );
    }
}
