package model

import "io"
import . "aicup2019/stream"

type Vec2Float32 struct {
    X float32
    Y float32
}
func NewVec2Float32(x float32, y float32) Vec2Float32 {
    return Vec2Float32 {
        X: x,
        Y: y,
    }
}
func ReadVec2Float32(reader io.Reader) Vec2Float32 {
    result := Vec2Float32 {}
    result.X = ReadFloat32(reader)
    result.Y = ReadFloat32(reader)
    return result
}
func (value Vec2Float32) Write(writer io.Writer) {
    WriteFloat32(writer, value.X)
    WriteFloat32(writer, value.Y)
}
