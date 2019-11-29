package model

import (
	"io"

	mStream "../stream"
)

type Vec2Float64 struct {
	X float64
	Y float64
}

func NewVec2Float64(x float64, y float64) *Vec2Float64 {
	return &Vec2Float64{
		X: x,
		Y: y,
	}
}
func ReadVec2Float64(reader io.Reader) *Vec2Float64 {
	return &Vec2Float64{
		X: mStream.ReadFloat64(reader),
		Y: mStream.ReadFloat64(reader),
	}
}
func (value *Vec2Float64) Write(writer io.Writer) {
	mStream.WriteFloat64(writer, value.X)
	mStream.WriteFloat64(writer, value.Y)
}
