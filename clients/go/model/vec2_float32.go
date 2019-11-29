package model

import (
	"io"

	mStream "../stream"
)

//Vec2Float32 -- universal struct for geometry
type Vec2Float32 struct {
	X float32
	Y float32
}

//NewVec2Float32 -- return link to new Vec2Float32
func NewVec2Float32(x float32, y float32) *Vec2Float32 {
	return &Vec2Float32{
		X: x,
		Y: y,
	}
}

//ReadVec2Float32 -- read Vec2Float32 from net from LocalRunner
func ReadVec2Float32(reader io.Reader) *Vec2Float32 {
	return &Vec2Float32{
		X: mStream.ReadFloat32(reader),
		Y: mStream.ReadFloat32(reader),
	}
}

//Write -- write Vec2Float32 to net to LocalRunner
func (value *Vec2Float32) Write(writer io.Writer) {
	mStream.WriteFloat32(writer, value.X)
	mStream.WriteFloat32(writer, value.Y)
}
