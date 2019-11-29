package model

import (
	"io"

	mStream "../stream"
)

type PlayerView struct {
	MyId int32
	Game *Game
}

func NewPlayerView(myId int32, game *Game) *PlayerView {
	return &PlayerView{
		MyId: myId,
		Game: game,
	}
}
func ReadPlayerView(reader io.Reader) *PlayerView {
	return &PlayerView{
		MyId: mStream.ReadInt32(reader),
		Game: ReadGame(reader),
	}
}
func (value *PlayerView) Write(writer io.Writer) {
	mStream.WriteInt32(writer, value.MyId)
	value.Game.Write(writer)
}
