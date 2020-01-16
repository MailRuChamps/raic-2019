use super::*;

pub struct WeaponRenderer {
    simple_renderer: Rc<SimpleRenderer>,
}

impl WeaponRenderer {
    pub fn new(simple_renderer: &Rc<SimpleRenderer>) -> Self {
        Self {
            simple_renderer: simple_renderer.clone(),
        }
    }
    pub fn draw(
        &self,
        framebuffer: &mut ugli::Framebuffer,
        camera: &Camera,
        rect: AABB<R64>,
        typ: model::WeaponType,
    ) {
        use model::WeaponType::*;
        self.simple_renderer.quad(
            framebuffer,
            camera,
            rect,
            match typ {
                Pistol => Color::rgb(0.0, 0.0, 0.5),
                AssaultRifle => Color::rgb(0.0, 0.0, 1.0),
                RocketLauncher => Color::rgb(1.0, 0.5, 0.0),
            },
        );
    }
}
