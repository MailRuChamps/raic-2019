package main

import (
	"bufio"

	mMod "./model"
)

//Debug -- screen print for debug my_strategy on LocalRunner
type Debug struct {
	Writer *bufio.Writer
}

//Draw -- draw text on screen of LocalRunner
func (debug *Debug) Draw(data mMod.CustomData) {
	PlayerMessageGameCustomDataMessage{
		Data: data,
	}.Write(debug.Writer)
	err := debug.Writer.Flush()
	if err != nil {
		panic(err)
	}
}
