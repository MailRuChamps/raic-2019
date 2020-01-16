use super::*;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Trans, Schematic)]
pub struct ExplosionParams {
    pub radius: R64,
    pub damage: i32,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Trans, Schematic)]
pub struct BulletParams {
    pub speed: R64,
    pub size: R64,
    pub damage: i32,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Trans, Schematic)]
pub struct WeaponParams {
    pub magazine_size: usize,
    pub fire_rate: R64,
    pub reload_time: R64,
    pub min_spread: R64,
    pub max_spread: R64,
    pub recoil: R64,
    pub aim_speed: R64,
    pub bullet: BulletParams,
    pub explosion: Option<ExplosionParams>,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, Eq, PartialEq, Hash, Trans, Schematic)]
pub enum WeaponType {
    Pistol,
    AssaultRifle,
    RocketLauncher,
}

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub struct Weapon {
    pub typ: WeaponType,
    pub params: WeaponParams,
    pub magazine: usize,
    pub was_shooting: bool,
    pub spread: R64,
    pub fire_timer: Option<R64>,
    pub last_angle: Option<R64>,
    pub last_fire_tick: Option<usize>,
}

impl Weapon {
    pub fn new(properties: &Properties, typ: WeaponType) -> Self {
        let params = properties.weapon_params.get(&typ).unwrap().clone();
        Self {
            typ,
            spread: params.min_spread,
            magazine: params.magazine_size,
            was_shooting: false,
            fire_timer: Some(params.reload_time),
            params,
            last_angle: None,
            last_fire_tick: None,
        }
    }
    pub fn reload(&mut self) {
        if self.magazine == self.params.magazine_size {
            return;
        }
        self.fire_timer = Some(self.params.reload_time);
        self.magazine = self.params.magazine_size;
    }
    pub fn set_shoot(&mut self, shoot: bool) {
        self.was_shooting = shoot;
    }
    pub fn update(&mut self, delta_time: R64) {
        self.spread = max(
            self.spread - self.params.aim_speed * delta_time,
            self.params.min_spread,
        );
        if let Some(timer) = self.fire_timer {
            let timer = timer - delta_time;
            if timer > r64(0.0) {
                self.fire_timer = Some(timer);
            } else {
                self.fire_timer = None;
            }
        }
    }
    pub fn aim(&mut self, direction: Vec2<R64>) {
        let angle = direction.arg();
        if let Some(last_angle) = self.last_angle {
            let mut extra_spread = angle - last_angle;
            while extra_spread >= R64::PI {
                extra_spread -= r64(2.0) * R64::PI;
            }
            while extra_spread < -R64::PI {
                extra_spread += r64(2.0) * R64::PI;
            }
            extra_spread = extra_spread.abs();
            self.spread = min(self.spread + extra_spread, self.params.max_spread);
        }
        self.last_angle = Some(angle);
    }
    pub fn fire(
        &mut self,
        current_tick: usize,
        unit_id: Id,
        player_id: Id,
        position: Vec2<R64>,
        direction: Vec2<R64>,
        rng: &mut dyn rand::RngCore,
    ) -> Option<Bullet> {
        if self.fire_timer.is_some() || self.magazine == 0 {
            return None;
        }
        self.last_fire_tick = Some(current_tick);
        self.magazine -= 1;
        let direction = Vec2::rotated(
            direction,
            r64(distributions::Uniform::from(-self.spread.raw()..=self.spread.raw()).sample(rng)),
        );
        self.spread = min(self.spread + self.params.recoil, self.params.max_spread);
        if self.magazine == 0 {
            self.magazine = self.params.magazine_size;
            self.fire_timer = Some(self.params.reload_time);
        } else {
            self.fire_timer = Some(self.params.fire_rate);
        }
        Some(Bullet {
            weapon_type: self.typ,
            unit_id,
            player_id,
            position,
            velocity: direction.normalize() * self.params.bullet.speed,
            damage: self.params.bullet.damage,
            size: self.params.bullet.size,
            explosion_params: self.params.explosion.clone(),
        })
    }
}
