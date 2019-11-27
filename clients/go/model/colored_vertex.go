package model

import "io"

type ColoredVertex struct {
    Position Vec2Float32
    Color ColorFloat32
}
func NewColoredVertex(position Vec2Float32, color ColorFloat32) ColoredVertex {
    return ColoredVertex {
        Position: position,
        Color: color,
    }
}
func ReadColoredVertex(reader io.Reader) ColoredVertex {
    result := ColoredVertex {}
    result.Position = ReadVec2Float32(reader)
    result.Color = ReadColorFloat32(reader)
    return result
}
func (value ColoredVertex) Write(writer io.Writer) {
    value.Position.Write(writer)
    value.Color.Write(writer)
}
