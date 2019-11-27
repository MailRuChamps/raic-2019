use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct Bullet {
    pub weapon_type: WeaponType,
    pub unit_id: i32,
    pub player_id: i32,
    pub position: Vec2F64,
    pub velocity: Vec2F64,
    pub damage: i32,
    pub size: f64,
    pub explosion_params: Option<ExplosionParams>,
}
