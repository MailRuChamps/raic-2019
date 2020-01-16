use super::*;

pub struct LootRenderer {
    simple_renderer: Rc<SimpleRenderer>,
    weapon_renderer: Rc<WeaponRenderer>,
}

impl LootRenderer {
    pub fn new(simple_renderer: &Rc<SimpleRenderer>, weapon_renderer: &Rc<WeaponRenderer>) -> Self {
        Self {
            simple_renderer: simple_renderer.clone(),
            weapon_renderer: weapon_renderer.clone(),
        }
    }
    pub fn draw(
        &mut self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        game: &model::Game,
    ) {
        for loot_box in &game.loot_boxes {
            match &loot_box.item {
                model::Item::HealthPack { .. } => {
                    self.simple_renderer
                        .quad(framebuffer, camera, loot_box.rect(), Color::GREEN);
                }
                model::Item::Weapon { weapon_type } => {
                    self.weapon_renderer
                        .draw(framebuffer, camera, loot_box.rect(), *weapon_type);
                }
                model::Item::Mine {} => {
                    self.simple_renderer
                        .quad(framebuffer, camera, loot_box.rect(), Color::YELLOW);
                }
            }
        }
    }
}
