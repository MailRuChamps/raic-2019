use super::*;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Trans, Schematic)]
pub enum MineState {
    Preparing,
    Idle,
    Triggered,
    Exploded,
}

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub struct Mine {
    pub player_id: Id,
    pub position: Vec2<R64>,
    pub size: Vec2<R64>,
    pub state: MineState,
    pub timer: Option<R64>,
    pub trigger_radius: R64,
    pub explosion_params: ExplosionParams,
}

impl Mine {
    pub fn spawn(player_id: Id, properties: &Properties, position: Vec2<R64>) -> Self {
        Self {
            player_id,
            position,
            size: properties.mine_size,
            state: MineState::Preparing,
            timer: Some(properties.mine_prepare_time),
            trigger_radius: properties.mine_trigger_radius,
            explosion_params: properties.mine_explosion_params.clone(),
        }
    }
    pub fn rect(&self) -> AABB<R64> {
        AABB::pos_size(
            self.position - vec2(self.size.x / r64(2.0), r64(0.0)),
            self.size,
        )
    }
}
