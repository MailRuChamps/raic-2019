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

//ColorFloat32 -- 32 bts color for drawing
type ColorFloat32 struct {
	R float32
	G float32
	B float32
	A float32
}

//NewColorFloat32 -- return link to new ColorFloat32
func NewColorFloat32(r float32, g float32, b float32, a float32) *ColorFloat32 {
	if r < 0 {
		panic(fmt.Errorf("NewColorFloat32(): FATAL ERROR r(%v)<0", r))
	}
	if g < 0 {
		panic(fmt.Errorf("NewColorFloat32(): FATAL ERROR g(%v)<0", g))
	}
	if b < 0 {
		panic(fmt.Errorf("NewColorFloat32(): FATAL ERROR b(%v)<0", b))
	}
	if a < 0 {
		panic(fmt.Errorf("NewColorFloat32(): FATAL ERROR a(%v)<0", a))
	}
	return &ColorFloat32{
		R: r,
		G: g,
		B: b,
		A: a,
	}
}

//ReadColorFloat32 -- read color from net connection from LocalRunner
func ReadColorFloat32(reader io.Reader) *ColorFloat32 {
	r := mStream.ReadFloat32(reader)
	if r < 0 {
		panic(fmt.Errorf("ReadColorFloat32(): FATAL ERROR r(%v)<0", r))
	}
	g := mStream.ReadFloat32(reader)
	if g < 0 {
		panic(fmt.Errorf("ReadColorFloat32(): FATAL ERROR g(%v)<0", g))
	}
	b := mStream.ReadFloat32(reader)
	if b < 0 {
		panic(fmt.Errorf("ReadColorFloat32(): FATAL ERROR b(%v)<0", b))
	}
	a := mStream.ReadFloat32(reader)
	if a < 0 {
		panic(fmt.Errorf("ReadColorFloat32(): FATAL ERROR a(%v)<0", a))
	}
	return &ColorFloat32{
		R: r,
		G: g,
		B: b,
		A: a,
	}
}

//Write -- write to net connection color to LocalRunner
func (value *ColorFloat32) Write(writer io.Writer) {
	mStream.WriteFloat32(writer, value.R)
	mStream.WriteFloat32(writer, value.G)
	mStream.WriteFloat32(writer, value.B)
	mStream.WriteFloat32(writer, value.A)
}
