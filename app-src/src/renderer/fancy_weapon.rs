use super::*;

#[derive(geng::Assets)]
pub struct Assets {
    #[asset(path = "assault_rifle/gun.png")]
    pub assault_rifle: ugli::Texture,
    #[asset(path = "assault_rifle/bullet.png")]
    pub assault_rifle_bullet: ugli::Texture,
    #[asset(path = "assault_rifle/muzzle_flash_*.png", range = "1..=2")]
    pub assault_rifle_muzzle_flash: Vec<ugli::Texture>,
    #[asset(path = "pistol/gun.png")]
    pub pistol: ugli::Texture,
    #[asset(path = "pistol/bullet.png")]
    pub pistol_bullet: ugli::Texture,
    #[asset(path = "pistol/muzzle_flash.png")]
    pub pistol_muzzle_flash: ugli::Texture,
    #[asset(path = "rocket_launcher/gun.png")]
    pub rocket_launcher: ugli::Texture,
    #[asset(path = "rocket_launcher/bullet.png")]
    pub rocket_launcher_bullet: ugli::Texture,
    #[asset(path = "rocket_launcher/muzzle_flash.png")]
    pub rocket_launcher_muzzle_flash: ugli::Texture,
}

impl Assets {
    pub fn setup(&mut self) {
        self.assault_rifle.set_filter(ugli::Filter::Nearest);
        for texture in &mut self.assault_rifle_muzzle_flash {
            texture.set_filter(ugli::Filter::Nearest);
        }
        self.pistol.set_filter(ugli::Filter::Nearest);
        self.pistol_muzzle_flash.set_filter(ugli::Filter::Nearest);
        self.rocket_launcher.set_filter(ugli::Filter::Nearest);
        self.rocket_launcher_muzzle_flash
            .set_filter(ugli::Filter::Nearest);
        self.pistol_bullet.set_filter(ugli::Filter::Nearest);
        self.assault_rifle_bullet.set_filter(ugli::Filter::Nearest);
        self.rocket_launcher_bullet
            .set_filter(ugli::Filter::Nearest);
    }
}
