use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct WeaponParams {
    pub magazine_size: i32,
    pub fire_rate: f64,
    pub reload_time: f64,
    pub min_spread: f64,
    pub max_spread: f64,
    pub recoil: f64,
    pub aim_speed: f64,
    pub bullet: BulletParams,
    pub explosion: Option<ExplosionParams>,
}
