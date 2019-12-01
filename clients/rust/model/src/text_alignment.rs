use crate::*;
#[derive(Clone, Debug, PartialEq, Eq, Hash, trans::Trans)]
pub enum TextAlignment {
    Left,
    Center,
    Right,
}
