package main

import . "aicup2019/model"
import "bufio"

type Debug struct {
	Writer *bufio.Writer
}

func (debug Debug) Draw(data CustomData) {
	PlayerMessageGameCustomDataMessage {
		Data: data,
	}.Write(debug.Writer)
	err := debug.Writer.Flush()
	if err != nil {
		panic(err)
	}
}