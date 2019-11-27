use crate::*;
#[derive(Clone, Debug, PartialEq, Eq, Hash, trans::Trans)]
pub enum Tile {
    Empty,
    Wall,
    Platform,
    Ladder,
    JumpPad,
}
