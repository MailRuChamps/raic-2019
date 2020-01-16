use super::*;

pub struct FancyBulletRenderer {
    sprite_renderer: Rc<SpriteRenderer>,
    weapon_assets: Rc<fancy_weapon::Assets>,
}

impl FancyBulletRenderer {
    pub fn new(
        sprite_renderer: &Rc<SpriteRenderer>,
        weapon_assets: &Rc<fancy_weapon::Assets>,
    ) -> Self {
        Self {
            sprite_renderer: sprite_renderer.clone(),
            weapon_assets: weapon_assets.clone(),
        }
    }
    pub fn draw(
        &mut self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        game: &model::Game,
    ) {
        let camera_uniforms = camera.uniforms(framebuffer);
        for bullet in &game.bullets {
            let texture = match bullet.weapon_type {
                model::WeaponType::Pistol => &self.weapon_assets.pistol_bullet,
                model::WeaponType::AssaultRifle => &self.weapon_assets.assault_rifle_bullet,
                model::WeaponType::RocketLauncher => &self.weapon_assets.rocket_launcher_bullet,
            };
            self.sprite_renderer.draw(
                framebuffer,
                &camera_uniforms,
                texture,
                vec2(0.5, 0.5),
                0.04,
                bullet.position.map(|x| x.raw() as f32),
                bullet.velocity.arg().raw() as f32,
                false,
                Color::WHITE,
            );
        }
    }
}
