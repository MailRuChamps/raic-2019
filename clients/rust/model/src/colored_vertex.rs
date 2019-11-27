use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct ColoredVertex {
    pub position: Vec2F32,
    pub color: ColorF32,
}
