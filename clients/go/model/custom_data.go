package model

import "io"
import . "aicup2019/stream"

type CustomData interface {
    Write(writer io.Writer)
}
func ReadCustomData(reader io.Reader) CustomData {
    switch ReadInt32(reader) {
        case 0:
            return ReadCustomDataLog(reader)
        case 1:
            return ReadCustomDataRect(reader)
        case 2:
            return ReadCustomDataLine(reader)
        case 3:
            return ReadCustomDataPolygon(reader)
    }
    panic("Unexpected discriminant value")
}

type CustomDataLog struct {
    Text string
}
func NewCustomDataLog(text string) CustomDataLog {
    return CustomDataLog {
        Text: text,
    }
}
func ReadCustomDataLog(reader io.Reader) CustomDataLog {
    result := CustomDataLog {}
    result.Text = ReadString(reader)
    return result
}
func (value CustomDataLog) Write(writer io.Writer) {
    WriteInt32(writer, 0)
    WriteString(writer, value.Text)
}

type CustomDataRect struct {
    Pos Vec2Float32
    Size Vec2Float32
    Color ColorFloat32
}
func NewCustomDataRect(pos Vec2Float32, size Vec2Float32, color ColorFloat32) CustomDataRect {
    return CustomDataRect {
        Pos: pos,
        Size: size,
        Color: color,
    }
}
func ReadCustomDataRect(reader io.Reader) CustomDataRect {
    result := CustomDataRect {}
    result.Pos = ReadVec2Float32(reader)
    result.Size = ReadVec2Float32(reader)
    result.Color = ReadColorFloat32(reader)
    return result
}
func (value CustomDataRect) Write(writer io.Writer) {
    WriteInt32(writer, 1)
    value.Pos.Write(writer)
    value.Size.Write(writer)
    value.Color.Write(writer)
}

type CustomDataLine struct {
    P1 Vec2Float32
    P2 Vec2Float32
    Width float32
    Color ColorFloat32
}
func NewCustomDataLine(p1 Vec2Float32, p2 Vec2Float32, width float32, color ColorFloat32) CustomDataLine {
    return CustomDataLine {
        P1: p1,
        P2: p2,
        Width: width,
        Color: color,
    }
}
func ReadCustomDataLine(reader io.Reader) CustomDataLine {
    result := CustomDataLine {}
    result.P1 = ReadVec2Float32(reader)
    result.P2 = ReadVec2Float32(reader)
    result.Width = ReadFloat32(reader)
    result.Color = ReadColorFloat32(reader)
    return result
}
func (value CustomDataLine) Write(writer io.Writer) {
    WriteInt32(writer, 2)
    value.P1.Write(writer)
    value.P2.Write(writer)
    WriteFloat32(writer, value.Width)
    value.Color.Write(writer)
}

type CustomDataPolygon struct {
    Vertices []ColoredVertex
}
func NewCustomDataPolygon(vertices []ColoredVertex) CustomDataPolygon {
    return CustomDataPolygon {
        Vertices: vertices,
    }
}
func ReadCustomDataPolygon(reader io.Reader) CustomDataPolygon {
    result := CustomDataPolygon {}
    result.Vertices = make([]ColoredVertex, ReadInt32(reader))
    for i := range result.Vertices {
        result.Vertices[i] = ReadColoredVertex(reader)
    }
    return result
}
func (value CustomDataPolygon) Write(writer io.Writer) {
    WriteInt32(writer, 3)
    WriteInt32(writer, int32(len(value.Vertices)))
    for _, VerticesElement := range value.Vertices {
        VerticesElement.Write(writer)
    }
}
