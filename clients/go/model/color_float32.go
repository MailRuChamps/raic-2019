package model

import (
	"io"

	. "../stream"
)

//ColorFloat32 -- 32 bts color for drawing
type ColorFloat32 struct {
	R float32
	G float32
	B float32
	A float32
}

//NewColorFloat32 -- return link to new ColorFloat32
func NewColorFloat32(r float32, g float32, b float32, a float32) *ColorFloat32 {
	return &ColorFloat32{
		R: r,
		G: g,
		B: b,
		A: a,
	}
}

//ReadColorFloat32 -- read color from net connection from LocalRunner
func ReadColorFloat32(reader io.Reader) ColorFloat32 {
	result := ColorFloat32{}
	result.R = ReadFloat32(reader)
	result.G = ReadFloat32(reader)
	result.B = ReadFloat32(reader)
	result.A = ReadFloat32(reader)
	return result
}

//Write -- write to net connection color to LocalRunner
func (value *ColorFloat32) Write(writer io.Writer) {
	WriteFloat32(writer, value.R)
	WriteFloat32(writer, value.G)
	WriteFloat32(writer, value.B)
	WriteFloat32(writer, value.A)
}
