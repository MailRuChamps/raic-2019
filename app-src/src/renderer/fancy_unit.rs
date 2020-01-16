use super::*;

#[derive(geng::Assets)]
pub struct PlayerAssets {
    #[asset(path = "jump/*.png", range = "1..=3")]
    jump: Vec<ugli::Texture>,
    #[asset(path = "rise/*.png", range = "1..=6")]
    rise: Vec<ugli::Texture>,
    #[asset(path = "walk/*.png", range = "1..=6")]
    walk: Vec<ugli::Texture>,
    #[asset(path = "hand.png")]
    hand: ugli::Texture,
    #[asset(path = "head.png")]
    head: ugli::Texture,
    #[asset(path = "stay.png")]
    stay: ugli::Texture,
}

impl PlayerAssets {
    fn setup(&mut self) {
        self.head.set_filter(ugli::Filter::Nearest);
        self.hand.set_filter(ugli::Filter::Nearest);
        self.stay.set_filter(ugli::Filter::Nearest);
        for texture in &mut self.jump {
            texture.set_filter(ugli::Filter::Nearest);
        }
        for texture in &mut self.rise {
            texture.set_filter(ugli::Filter::Nearest);
        }
        for texture in &mut self.walk {
            texture.set_filter(ugli::Filter::Nearest);
        }
    }
}

#[derive(geng::Assets)]
pub struct Assets {
    #[asset(path = "*", range = "1..=3")]
    players: Vec<PlayerAssets>,
}

impl Assets {
    fn setup(&mut self) {
        for player in &mut self.players {
            player.setup();
        }
    }
}

pub struct FancyUnitRenderer {
    simple_renderer: Rc<SimpleRenderer>,
    sprite_renderer: Rc<SpriteRenderer>,
    assets: Assets,
    weapon_assets: Rc<fancy_weapon::Assets>,
}

impl FancyUnitRenderer {
    const ANIMATION_SPEED: f64 = 1.2;
    pub fn new(
        geng: &Rc<Geng>,
        simple_renderer: &Rc<SimpleRenderer>,
        sprite_renderer: &Rc<SpriteRenderer>,
        mut assets: Assets,
        weapon_assets: &Rc<fancy_weapon::Assets>,
    ) -> Self {
        assets.setup();
        Self {
            simple_renderer: simple_renderer.clone(),
            sprite_renderer: sprite_renderer.clone(),
            assets,
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
        for unit in &game.units {
            let player_assets = &self.assets.players[unit.id.raw() % self.assets.players.len()];
            let texture = &player_assets.walk[0];
            let mut angle = unit
                .weapon
                .as_ref()
                .and_then(|weapon| weapon.last_angle)
                .map(|angle| angle.raw() as f32)
                .unwrap_or(if unit.walked_right {
                    0.0
                } else {
                    std::f32::consts::PI
                });
            let flip = angle > std::f32::consts::PI / 2.0 || angle < -std::f32::consts::PI / 2.0;
            if flip {
                if angle < 0.0 {
                    angle = angle + std::f32::consts::PI;
                } else {
                    angle = angle - std::f32::consts::PI;
                }
            }
            let height = unit.size.y.raw() as f32 * 0.6;
            let texture_scale = height / texture.size().y as f32;
            // body
            {
                let mut y_off = 0.0;
                let texture = if unit.on_ladder {
                    y_off = height * 0.8;
                    let index = (unit.position.y.raw() * Self::ANIMATION_SPEED) as usize
                        % player_assets.rise.len();
                    &player_assets.rise[index]
                } else if unit.on_ground {
                    if unit.stand {
                        &player_assets.stay
                    } else {
                        let index = (unit.position.x.raw() * Self::ANIMATION_SPEED) as usize
                            % player_assets.walk.len();
                        &player_assets.walk[index]
                    }
                } else {
                    &player_assets.jump[if unit.jump_state.can_jump { 0 } else { 2 }]
                };
                self.sprite_renderer.draw(
                    framebuffer,
                    &camera_uniforms,
                    texture,
                    vec2(0.5, 1.0),
                    texture_scale,
                    vec2(
                        unit.position.x.raw() as f32,
                        unit.position.y.raw() as f32
                            + unit.size.y.raw() as f32 * 0.6
                            + y_off as f32,
                    ),
                    0.0,
                    flip,
                    Color::WHITE,
                );
            }
            // head
            {
                self.sprite_renderer.draw(
                    framebuffer,
                    &camera_uniforms,
                    &player_assets.head,
                    vec2(0.5, 0.3),
                    texture_scale,
                    vec2(
                        unit.position.x.raw() as f32,
                        unit.position.y.raw() as f32 + unit.size.y.raw() as f32 * 0.75,
                    ),
                    angle / 2.0,
                    flip,
                    Color::WHITE,
                );
            }
            // weapon
            if let Some(weapon) = &unit.weapon {
                let (texture, origin) = match weapon.typ {
                    model::WeaponType::Pistol => (&self.weapon_assets.pistol, vec2(-0.3, 0.8)),
                    model::WeaponType::AssaultRifle => {
                        (&self.weapon_assets.assault_rifle, vec2(-0.2, 0.6))
                    }
                    model::WeaponType::RocketLauncher => {
                        (&self.weapon_assets.rocket_launcher, vec2(0.3, 0.5))
                    }
                };
                self.sprite_renderer.draw(
                    framebuffer,
                    &camera_uniforms,
                    texture,
                    origin,
                    texture_scale,
                    vec2(
                        unit.position.x.raw() as f32
                            - if flip { -1.0 } else { 1.0 } * unit.size.x.raw() as f32 * 0.1,
                        unit.position.y.raw() as f32 + unit.size.y.raw() as f32 * 0.55,
                    ),
                    angle,
                    flip,
                    Color::WHITE,
                );
                if let Some(tick) = weapon.last_fire_tick {
                    let muzzle_flash_index = (game.current_tick - tick) / 2;
                    if muzzle_flash_index < 2 {
                        let (texture, origin) = match weapon.typ {
                            model::WeaponType::Pistol => {
                                (&self.weapon_assets.pistol_muzzle_flash, vec2(-0.9, 0.5))
                            }
                            model::WeaponType::AssaultRifle => (
                                &self.weapon_assets.assault_rifle_muzzle_flash[muzzle_flash_index],
                                vec2(-1.2, 0.5),
                            ),
                            model::WeaponType::RocketLauncher => (
                                &self.weapon_assets.rocket_launcher_muzzle_flash,
                                vec2(-1.8, 0.5),
                            ),
                        };
                        self.sprite_renderer.draw(
                            framebuffer,
                            &camera_uniforms,
                            texture,
                            origin,
                            texture_scale,
                            vec2(
                                unit.position.x.raw() as f32
                                    - if flip { -1.0 } else { 1.0 }
                                        * unit.size.x.raw() as f32
                                        * 0.1,
                                unit.position.y.raw() as f32 + unit.size.y.raw() as f32 * 0.55,
                            ),
                            angle,
                            flip,
                            Color::WHITE,
                        );
                    }
                }
            }
            //hand
            self.sprite_renderer.draw(
                framebuffer,
                &camera_uniforms,
                &player_assets.hand,
                vec2(0.1, 0.9),
                texture_scale,
                vec2(
                    unit.position.x.raw() as f32
                        - if flip { -1.0 } else { 1.0 } * unit.size.x.raw() as f32 * 0.1,
                    unit.position.y.raw() as f32 + unit.size.y.raw() as f32 * 0.55,
                ),
                angle,
                flip,
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
                                - r64(unit.health as f64 / game.properties.unit_max_health as f64)),
                    ),
                ),
                Color::rgba(1.0, 0.0, 0.0, 0.5),
            );
        }
    }
}
