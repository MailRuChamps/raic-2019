use super::*;

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
#[cfg_attr(feature = "rendering", derive(ugli::Vertex))]
pub struct ColoredVertex {
    pub position: Vec2<f32>,
    pub color: Color<f32>,
}

#[derive(Debug, Serialize, Deserialize, Copy, Clone, Trans, Schematic)]
pub enum TextAlignment {
    Left,
    Center,
    Right,
}

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub enum CustomData {
    Log {
        text: String,
    },
    Rect {
        pos: Vec2<f32>,
        size: Vec2<f32>,
        color: Color<f32>,
    },
    Line {
        p1: Vec2<f32>,
        p2: Vec2<f32>,
        width: f32,
        color: Color<f32>,
    },
    Polygon {
        vertices: Vec<ColoredVertex>,
    },
    PlacedText {
        text: String,
        pos: Vec2<f32>,
        alignment: TextAlignment,
        size: f32,
        color: Color<f32>,
    },
}
