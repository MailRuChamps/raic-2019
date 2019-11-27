use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct BulletParams {
    pub speed: f64,
    pub size: f64,
    pub damage: i32,
}
