use super::*;

#[derive(geng::Assets)]
pub struct Assets {
    #[asset(path = "health_pack.png")]
    pub health_pack: ugli::Texture,
    #[asset(path = "mine.png")]
    pub mine: ugli::Texture,
    #[asset(path = "pistol.png")]
    pub pistol: ugli::Texture,
    #[asset(path = "assault_rifle.png")]
    pub assault_rifle: ugli::Texture,
    #[asset(path = "rocket_launcher.png")]
    pub rocket_launcher: ugli::Texture,
}

impl Assets {
    fn setup(&mut self) {
        self.health_pack.set_filter(ugli::Filter::Nearest);
        self.mine.set_filter(ugli::Filter::Nearest);
        self.pistol.set_filter(ugli::Filter::Nearest);
        self.assault_rifle.set_filter(ugli::Filter::Nearest);
        self.rocket_launcher.set_filter(ugli::Filter::Nearest);
    }
}

pub struct FancyLootRenderer {
    sprite_renderer: Rc<SpriteRenderer>,
    assets: Assets,
}

impl FancyLootRenderer {
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
        for loot_box in &game.loot_boxes {
            let texture = match &loot_box.item {
                model::Item::HealthPack { .. } => &self.assets.health_pack,
                model::Item::Weapon { weapon_type } => match weapon_type {
                    model::WeaponType::Pistol => &self.assets.pistol,
                    model::WeaponType::AssaultRifle => &self.assets.assault_rifle,
                    model::WeaponType::RocketLauncher => &self.assets.rocket_launcher,
                },
                model::Item::Mine {} => &self.assets.mine,
            };
            self.sprite_renderer.draw(
                framebuffer,
                &camera_uniforms,
                texture,
                vec2(0.5, 0.0),
                0.04,
                loot_box.position.map(|x| x.raw() as f32),
                0.0,
                false,
                Color::WHITE,
            );
        }
    }
}
