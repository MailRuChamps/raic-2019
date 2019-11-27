use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct Mine {
    pub player_id: i32,
    pub position: Vec2F64,
    pub size: Vec2F64,
    pub state: MineState,
    pub timer: Option<f64>,
    pub trigger_radius: f64,
    pub explosion_params: ExplosionParams,
}
