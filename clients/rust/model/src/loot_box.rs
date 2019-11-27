use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct LootBox {
    pub position: Vec2F64,
    pub size: Vec2F64,
    pub item: Item,
}
