use super::*;

pub struct UnitRenderer {
    geng: Rc<Geng>,
    simple_renderer: Rc<SimpleRenderer>,
    weapon_renderer: Rc<WeaponRenderer>,
}

impl UnitRenderer {
    pub fn new(
        geng: &Rc<Geng>,
        simple_renderer: &Rc<SimpleRenderer>,
        weapon_renderer: &Rc<WeaponRenderer>,
    ) -> Self {
        Self {
            geng: geng.clone(),
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
        for unit in &game.units {
            self.simple_renderer.quad(
                framebuffer,
                camera,
                AABB::pos_size(
                    unit.position - vec2(unit.size.x / r64(2.0), r64(0.0)),
                    unit.size,
                ),
                Color::WHITE,
            );
            self.simple_renderer.quad(
                framebuffer,
                camera,
                AABB::pos_size(
                    unit.position - vec2(unit.size.x / r64(2.0), r64(0.0)),
                    vec2(
                        unit.size.x,
                        unit.size.y
                            * (r64(1.0)
                                - r64(unit.health as f64)
                                    / r64(game.properties.unit_max_health as f64)),
                    ),
                ),
                Color::RED,
            );
            if let Some(weapon) = &unit.weapon {
                self.weapon_renderer.draw(
                    framebuffer,
                    camera,
                    AABB::pos_size(
                        unit.rect().center() - vec2(r64(0.25), r64(0.25)),
                        vec2(r64(0.5), r64(0.5)),
                    ),
                    weapon.typ,
                );
                if let Some(angle) = weapon.last_angle {
                    let vertices = ugli::VertexBuffer::new_static(
                        &self.geng.ugli(),
                        vec![
                            simple::Vertex {
                                a_pos: vec2(0.0, 0.0),
                            },
                            simple::Vertex {
                                a_pos: Vec2::rotated(
                                    vec2(100.0, 0.0),
                                    (angle + weapon.spread).raw() as f32,
                                ),
                            },
                            simple::Vertex {
                                a_pos: Vec2::rotated(
                                    vec2(100.0, 0.0),
                                    (angle - weapon.spread).raw() as f32,
                                ),
                            },
                        ],
                    );
                    let instances = ugli::VertexBuffer::new_static(
                        &self.geng.ugli(),
                        vec![simple::Instance {
                            i_pos: unit.center().map(|x| x.raw() as f32),
                            i_size: vec2(1.0, 1.0),
                            i_color: Color::rgba(1.0, 1.0, 1.0, 0.1),
                        }],
                    );
                    self.simple_renderer
                        .draw(framebuffer, camera, &vertices, &instances, None);
                }
            }
        }
    }
}
