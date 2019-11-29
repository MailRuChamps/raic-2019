package model

import (
	"io"
)

//CustomData -- custom data for games object
type CustomData interface {
	Write(writer io.Writer)
}

const (
	iDataLog     = 0
	iDataRect    = 1
	iDataLine    = 2
	iDataPolygon = 3
)

//ReadCustomData -- reads CustomData from net from LocalRunner
func ReadCustomData(reader io.Reader) CustomData {
	switch ReadInt32(reader) {
	case iDataLog:
		return ReadCustomDataLog(reader)
	case iDataRect:
		return ReadCustomDataRect(reader)
	case iDataLine:
		return ReadCustomDataLine(reader)
	case iDataPolygon:
		return ReadCustomDataPolygon(reader)
	}
	panic("Unexpected discriminant value")
}

//CustomDataLog -- custom log for data
type CustomDataLog struct {
	Text string
}

//NewCustomDataLog -- return link to new CustomDataLog
func NewCustomDataLog(text string) *CustomDataLog {
	return &CustomDataLog{
		Text: text,
	}
}

//ReadCustomDataLog -- read CustomDataLog from net from LocalRunner
func ReadCustomDataLog(reader io.Reader) *CustomDataLog {
	return &CustomDataLog{
		Text: ReadString(reader),
	}
}

//Write -- write to net CustomDataLog from LocalRunner
func (value *CustomDataLog) Write(writer io.Writer) {
	WriteInt32(writer, 0)
	WriteString(writer, value.Text)
}

//CustomDataRect -- rec tangle for object on fields gane
type CustomDataRect struct {
	Pos   Vec2Float32
	Size  Vec2Float32
	Color ColorFloat32
}

//NewCustomDataRect -- return link to new CustomDataRect
func NewCustomDataRect(pos Vec2Float32, size Vec2Float32, color ColorFloat32) *CustomDataRect {
	return &CustomDataRect{
		Pos:   pos,
		Size:  size,
		Color: color,
	}
}

//ReadCustomDataRect -- read from net CustomDataRect from LocalRunner
func ReadCustomDataRect(reader io.Reader) *CustomDataRect {
	return &CustomDataRect{
		Pos:   ReadVec2Float32(reader),
		Size:  ReadVec2Float32(reader),
		Color: ReadColorFloat32(reader),
	}
}

//Write -- write CustomDataRect to net to LocalRunner
func (value *CustomDataRect) Write(writer io.Writer) {
	WriteInt32(writer, 1)
	value.Pos.Write(writer)
	value.Size.Write(writer)
	value.Color.Write(writer)
}

//CustomDataLine -- custom data for line on feilds game
type CustomDataLine struct {
	P1    Vec2Float32
	P2    Vec2Float32
	Width float32
	Color ColorFloat32
}

//NewCustomDataLine -- return link to new CustomDataLine
func NewCustomDataLine(p1 Vec2Float32, p2 Vec2Float32, width float32, color ColorFloat32) *CustomDataLine {
	return &CustomDataLine{
		P1:    p1,
		P2:    p2,
		Width: width,
		Color: color,
	}
}

//ReadCustomDataLine -- read CustomDataLine from net  from LocalRunner
func ReadCustomDataLine(reader io.Reader) *CustomDataLine {
	return &CustomDataLine{
		P1:    ReadVec2Float32(reader),
		P2:    ReadVec2Float32(reader),
		Width: ReadFloat32(reader),
		Color: ReadColorFloat32(reader),
	}
}

//Write -- write CustomDataLine to net to LocalRunner
func (value CustomDataLine) Write(writer io.Writer) {
	WriteInt32(writer, 2)
	value.P1.Write(writer)
	value.P2.Write(writer)
	WriteFloat32(writer, value.Width)
	value.Color.Write(writer)
}

//CustomDataPolygon -- custom data for polygon for field games
type CustomDataPolygon struct {
	Vertices []ColoredVertex
}

//NewCustomDataPolygon -- return link to new CustomDataPolygon
func NewCustomDataPolygon(vertices []ColoredVertex) *CustomDataPolygon {
	return &CustomDataPolygon{
		Vertices: vertices,
	}
}

//ReadCustomDataPolygon -- read CustomDataPolygon from net from LocalRunner
func ReadCustomDataPolygon(reader io.Reader) *CustomDataPolygon {
	result := &CustomDataPolygon{
		Vertices: make([]ColoredVertex, ReadInt32(reader)),
	}
	for i := range result.Vertices {
		result.Vertices[i] = ReadColoredVertex(reader)
	}
	return result
}

//Write -- write CustomDataPolygon to net to LocalRunner
func (value *CustomDataPolygon) Write(writer io.Writer) {
	WriteInt32(writer, 3)
	WriteInt32(writer, int32(len(value.Vertices)))
	for _, VerticesElement := range value.Vertices {
		VerticesElement.Write(writer)
	}
}
