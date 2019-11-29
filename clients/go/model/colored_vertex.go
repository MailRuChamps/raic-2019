package model

/*
	DO NOT CHANGE this module.
	Its automatic replaced on games server
*/

import (
	"fmt"
	"io"
)

//ColoredVertex -- colored vertex for drawing
type ColoredVertex struct {
	Position *Vec2Float32
	Color    *ColorFloat32
}

//NewColoredVertex -- return link to new ColoredVertex
func NewColoredVertex(position *Vec2Float32, color *ColorFloat32) *ColoredVertex {
	if position == nil {
		panic(fmt.Errorf("NewColoredVertex(): FATAL ERROR position==nil"))
	}
	if color == nil {
		panic(fmt.Errorf("NewColoredVertex(): FATAL ERROR color==nil"))
	}
	return &ColoredVertex{
		Position: position,
		Color:    color,
	}
}

//ReadColoredVertex -- read from net connection ColoredVertex
func ReadColoredVertex(reader io.Reader) *ColoredVertex {
	pos := ReadVec2Float32(reader)
	if pos == nil {
		panic(fmt.Errorf("ReadColoredVertex(): FATAL ERROR pos==nil"))
	}
	col := ReadColorFloat32(reader)
	if col == nil {
		panic(fmt.Errorf("ReadColoredVertex(): FATAL ERROR col==nil"))
	}
	return &ColoredVertex{
		Position: pos,
		Color:    col,
	}
}

//Write -- write ColoredVertex to net connection to LocalRunner
func (value *ColoredVertex) Write(writer io.Writer) {
	value.Position.Write(writer)
	value.Color.Write(writer)
}
