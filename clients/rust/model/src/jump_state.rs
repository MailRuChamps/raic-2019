use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct JumpState {
    pub can_jump: bool,
    pub speed: f64,
    pub max_time: f64,
    pub can_cancel: bool,
}
