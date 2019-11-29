package model

/*
	DO NOT CHANGE this module.
	Its automatic replaced on games server
*/

import (
	"fmt"
	"io"

	mStream "../stream"
)

//ICustomData -- custom data for games object
type ICustomData interface {
	Write(writer io.Writer)
}

const (
	iDataLog     = 0
	iDataRect    = 1
	iDataLine    = 2
	iDataPolygon = 3
)

//ReadCustomData -- reads ICustomData from net from LocalRunner
func ReadCustomData(reader io.Reader) (cd ICustomData) {
	switch mStream.ReadInt32(reader) {
	case iDataLog:
		cd = ReadCustomDataLog(reader)
	case iDataRect:
		cd = ReadCustomDataRect(reader)
	case iDataLine:
		cd = ReadCustomDataLine(reader)
	case iDataPolygon:
		cd = ReadCustomDataPolygon(reader)
	default:
		panic("Unexpected discriminant value")
	}
	if cd == nil {
		panic(fmt.Errorf("ReadCustomData(): FATAL ERROR cd==nil"))
	}
	return cd
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
func ReadCustomDataLog(reader io.Reader) ICustomData {
	return &CustomDataLog{
		Text: mStream.ReadString(reader),
	}
}

//Write -- write to net CustomDataLog from LocalRunner
func (value *CustomDataLog) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 0)
	mStream.WriteString(writer, value.Text)
}

//CustomDataRect -- rec tangle for object on fields gane
type CustomDataRect struct {
	Pos   *Vec2Float32
	Size  *Vec2Float32
	Color *ColorFloat32
}

//NewCustomDataRect -- return link to new CustomDataRect
func NewCustomDataRect(pos *Vec2Float32, size *Vec2Float32, color *ColorFloat32) *CustomDataRect {
	if pos == nil {
		panic(fmt.Errorf("NewCustomDataRect(): FATAL ERROR pos==nil"))
	}
	if size == nil {
		panic(fmt.Errorf("NewCustomDataRect(): FATAL ERROR size==nil"))
	}
	if color == nil {
		panic(fmt.Errorf("NewCustomDataRect(): FATAL ERROR color==nil"))
	}
	return &CustomDataRect{
		Pos:   pos,
		Size:  size,
		Color: color,
	}
}

//ReadCustomDataRect -- read from net CustomDataRect from LocalRunner
func ReadCustomDataRect(reader io.Reader) *CustomDataRect {
	pos := ReadVec2Float32(reader)
	if pos == nil {
		panic(fmt.Errorf("ReadCustomDataRect(): FATAL ERROR pos==nil"))
	}
	size := ReadVec2Float32(reader)
	if size == nil {
		panic(fmt.Errorf("ReadCustomDataRect(): FATAL ERROR size==nil"))
	}
	color := ReadColorFloat32(reader)
	if color == nil {
		panic(fmt.Errorf("ReadCustomDataRect(): FATAL ERROR color==nil"))
	}
	return &CustomDataRect{
		Pos:   pos,
		Size:  size,
		Color: color,
	}
}

//Write -- write CustomDataRect to net to LocalRunner
func (value *CustomDataRect) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 1)
	value.Pos.Write(writer)
	value.Size.Write(writer)
	value.Color.Write(writer)
}

//CustomDataLine -- custom data for line on feilds game
type CustomDataLine struct {
	P1    *Vec2Float32
	P2    *Vec2Float32
	Width float32
	Color *ColorFloat32
}

//NewCustomDataLine -- return link to new CustomDataLine
func NewCustomDataLine(p1 *Vec2Float32, p2 *Vec2Float32, width float32, color *ColorFloat32) *CustomDataLine {
	if p1 == nil {
		panic(fmt.Errorf("NewCustomDataLine(): FATAL ERROR p1==nil"))
	}
	if p2 == nil {
		panic(fmt.Errorf("NewCustomDataLine(): FATAL ERROR p2==nil"))
	}
	if width < 0 {
		panic(fmt.Errorf("NewCustomDataLine(): FATAL ERROR width(%v)<0", width))
	}
	if color == nil {
		panic(fmt.Errorf("NewCustomDataLine(): FATAL ERROR color==nil"))
	}
	return &CustomDataLine{
		P1:    p1,
		P2:    p2,
		Width: width,
		Color: color,
	}
}

//ReadCustomDataLine -- read CustomDataLine from net  from LocalRunner
func ReadCustomDataLine(reader io.Reader) *CustomDataLine {
	p1 := ReadVec2Float32(reader)
	if p1 == nil {
		panic(fmt.Errorf("ReadCustomDataLine(): FATAL ERROR p1==nil"))
	}
	p2 := ReadVec2Float32(reader)
	if p2 == nil {
		panic(fmt.Errorf("ReadCustomDataLine(): FATAL ERROR p2==nil"))
	}
	width := mStream.ReadFloat32(reader)
	if width < 0 {
		panic(fmt.Errorf("ReadCustomDataLine(): FATAL ERROR width(%v)<0", width))
	}
	color := ReadColorFloat32(reader)
	if color == nil {
		panic(fmt.Errorf("ReadCustomDataLine(): FATAL ERROR color==nil"))
	}
	return &CustomDataLine{
		P1:    p1,
		P2:    p2,
		Width: width,
		Color: color,
	}
}

//Write -- write CustomDataLine to net to LocalRunner
func (value CustomDataLine) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 2)
	value.P1.Write(writer)
	value.P2.Write(writer)
	mStream.WriteFloat32(writer, value.Width)
	value.Color.Write(writer)
}

//CustomDataPolygon -- custom data for polygon for field games
type CustomDataPolygon struct {
	Vertices []*ColoredVertex
}

//NewCustomDataPolygon -- return link to new CustomDataPolygon
func NewCustomDataPolygon(vertices []*ColoredVertex) *CustomDataPolygon {
	if vertices == nil {
		panic(fmt.Errorf("NewCustomDataPolygon(): FATAL ERROR vertices==nil"))
	}
	return &CustomDataPolygon{
		Vertices: vertices,
	}
}

//ReadCustomDataPolygon -- read CustomDataPolygon from net from LocalRunner
func ReadCustomDataPolygon(reader io.Reader) *CustomDataPolygon {
	result := &CustomDataPolygon{
		Vertices: make([]*ColoredVertex, mStream.ReadInt32(reader)),
	}
	for i := range result.Vertices {
		result.Vertices[i] = ReadColoredVertex(reader)
	}
	return result
}

//Write -- write CustomDataPolygon to net to LocalRunner
func (value *CustomDataPolygon) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 3)
	mStream.WriteInt32(writer, int32(len(value.Vertices)))
	for _, VerticesElement := range value.Vertices {
		VerticesElement.Write(writer)
	}
}
