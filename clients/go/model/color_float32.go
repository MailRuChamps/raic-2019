package model

import "io"
import . "aicup2019/stream"

type ColorFloat32 struct {
    R float32
    G float32
    B float32
    A float32
}
func NewColorFloat32(r float32, g float32, b float32, a float32) ColorFloat32 {
    return ColorFloat32 {
        R: r,
        G: g,
        B: b,
        A: a,
    }
}
func ReadColorFloat32(reader io.Reader) ColorFloat32 {
    result := ColorFloat32 {}
    result.R = ReadFloat32(reader)
    result.G = ReadFloat32(reader)
    result.B = ReadFloat32(reader)
    result.A = ReadFloat32(reader)
    return result
}
func (value ColorFloat32) Write(writer io.Writer) {
    WriteFloat32(writer, value.R)
    WriteFloat32(writer, value.G)
    WriteFloat32(writer, value.B)
    WriteFloat32(writer, value.A)
}
