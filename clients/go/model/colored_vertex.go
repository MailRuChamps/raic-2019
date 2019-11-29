package model

import (
	"io"
)

//ColoredVertex -- colored vertex for drawing
type ColoredVertex struct {
	Position *Vec2Float32
	Color    *ColorFloat32
}

//NewColoredVertex -- return link to new ColoredVertex
func NewColoredVertex(position *Vec2Float32, color *ColorFloat32) *ColoredVertex {
	return &ColoredVertex{
		Position: position,
		Color:    color,
	}
}

//ReadColoredVertex -- read from net connection ColoredVertex
func ReadColoredVertex(reader io.Reader) *ColoredVertex {
	return &ColoredVertex{
		Position: ReadVec2Float32(reader),
		Color:    ReadColorFloat32(reader),
	}
}

//Write -- write ColoredVertex to net connection to LocalRunner
func (value *ColoredVertex) Write(writer io.Writer) {
	value.Position.Write(writer)
	value.Color.Write(writer)
}
