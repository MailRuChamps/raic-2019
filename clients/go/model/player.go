package model

import (
	mStream "../stream"
	"io"
)

//Player -- object of player on game field
type Player struct {
	Id    int32
	Score int32
}

//NewPlayer -- retun link to new Player
func NewPlayer(id int32, score int32) *Player {
	return &Player{
		Id:    id,
		Score: score,
	}
}

//ReadPlayer -- read Player from net connection from LocalRunner
func ReadPlayer(reader io.Reader) *Player {
	result := &Player{}
	result.Id = mStream.ReadInt32(reader)
	result.Score = mStream.ReadInt32(reader)
	return result
}

//Write -- write Player to net connection to LocalRunner
func (value *Player) Write(writer io.Writer) {
	mStream.WriteInt32(writer, value.Id)
	mStream.WriteInt32(writer, value.Score)
}
