use super::*;

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub struct JumpState {
    pub can_jump: bool,
    pub speed: R64,
    pub max_time: R64,
    pub can_cancel: bool,
}

impl JumpState {
    pub fn falling() -> Self {
        Self {
            can_jump: false,
            speed: r64(0.0),
            max_time: r64(0.0),
            can_cancel: false,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub struct Unit {
    pub player_id: Id,
    pub id: Id,
    pub health: i32,
    pub position: Vec2<R64>,
    pub size: Vec2<R64>,
    pub jump_state: JumpState,
    pub walked_right: bool,
    pub stand: bool,
    pub on_ground: bool,
    pub on_ladder: bool,
    pub mines: usize,
    pub weapon: Option<Weapon>,
}

impl Unit {
    pub(crate) fn spawn(properties: &Properties, player_id: Id, position: Vec2<R64>) -> Self {
        Self {
            player_id,
            id: Id::new(),
            health: properties.unit_max_health,
            position,
            size: properties.unit_size,
            jump_state: JumpState::falling(),
            mines: 0,
            weapon: None,
            walked_right: true,
            stand: true,
            on_ground: false,
            on_ladder: false,
        }
    }
    pub fn rect(&self) -> AABB<R64> {
        self.rect_at(self.position)
    }
    pub fn rect_at(&self, position: Vec2<R64>) -> AABB<R64> {
        AABB::pos_size(position - vec2(self.size.x / r64(2.0), r64(0.0)), self.size)
    }
    pub fn center(&self) -> Vec2<R64> {
        self.position + vec2(r64(0.0), self.size.y / r64(2.0))
    }
}
