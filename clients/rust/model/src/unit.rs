use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct Unit {
    pub player_id: i32,
    pub id: i32,
    pub health: i32,
    pub position: Vec2F64,
    pub size: Vec2F64,
    pub jump_state: JumpState,
    pub walked_right: bool,
    pub stand: bool,
    pub on_ground: bool,
    pub on_ladder: bool,
    pub mines: i32,
    pub weapon: Option<Weapon>,
}
