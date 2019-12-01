use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub enum CustomData {
    Log {
        text: String,
    },
    Rect {
        pos: Vec2F32,
        size: Vec2F32,
        color: ColorF32,
    },
    Line {
        p1: Vec2F32,
        p2: Vec2F32,
        width: f32,
        color: ColorF32,
    },
    Polygon {
        vertices: Vec<ColoredVertex>,
    },
    PlacedText {
        text: String,
        pos: Vec2F32,
        alignment: TextAlignment,
        size: f32,
        color: ColorF32,
    },
}
