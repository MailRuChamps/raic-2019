package model

import (
	"io"
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
		X: ReadFloat64(reader),
		Y: ReadFloat64(reader),
	}
}
func (value *Vec2Float64) Write(writer io.Writer) {
	WriteFloat64(writer, value.X)
	WriteFloat64(writer, value.Y)
}
