use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct UnitAction {
    pub velocity: f64,
    pub jump: bool,
    pub jump_down: bool,
    pub aim: Vec2F64,
    pub shoot: bool,
    pub reload: bool,
    pub swap_weapon: bool,
    pub plant_mine: bool,
}
