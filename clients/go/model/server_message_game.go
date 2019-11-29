package model

import (
	"io"

	mStream "../stream"
)

type ServerMessageGame struct {
	PlayerView *PlayerView
}

func NewServerMessageGame(playerView *PlayerView) ServerMessageGame {
	return ServerMessageGame{
		PlayerView: playerView,
	}
}
func ReadServerMessageGame(reader io.Reader) ServerMessageGame {
	result := ServerMessageGame{}
	if mStream.ReadBool(reader) {
		result.PlayerView = ReadPlayerView(reader)
	} else {
		result.PlayerView = nil
	}
	return result
}
func (value ServerMessageGame) Write(writer io.Writer) {
	if value.PlayerView == nil {
		mStream.WriteBool(writer, false)
	} else {
		mStream.WriteBool(writer, true)
		(*value.PlayerView).Write(writer)
	}
}
