use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct Level {
    pub tiles: Vec<Vec<Tile>>,
}
