use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct Weapon {
    pub typ: WeaponType,
    pub params: WeaponParams,
    pub magazine: i32,
    pub was_shooting: bool,
    pub spread: f64,
    pub fire_timer: Option<f64>,
    pub last_angle: Option<f64>,
    pub last_fire_tick: Option<i32>,
}
