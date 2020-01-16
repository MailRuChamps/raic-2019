use super::*;

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub struct Bullet {
    pub weapon_type: WeaponType,
    pub unit_id: Id,
    pub player_id: Id,
    pub position: Vec2<R64>,
    pub velocity: Vec2<R64>,
    pub damage: i32,
    pub size: R64,
    pub explosion_params: Option<ExplosionParams>,
}

impl Bullet {
    pub fn rect(&self) -> AABB<R64> {
        AABB::pos_size(
            self.position - vec2(self.size, self.size) / r64(2.0),
            vec2(self.size, self.size),
        )
    }
}
