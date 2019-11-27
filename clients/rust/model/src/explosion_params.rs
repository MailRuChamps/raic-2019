use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct ExplosionParams {
    pub radius: f64,
    pub damage: i32,
}
