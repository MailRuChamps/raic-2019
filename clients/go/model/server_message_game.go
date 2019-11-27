package model

import "io"
import . "aicup2019/stream"

type ServerMessageGame struct {
    PlayerView *PlayerView
}
func NewServerMessageGame(playerView *PlayerView) ServerMessageGame {
    return ServerMessageGame {
        PlayerView: playerView,
    }
}
func ReadServerMessageGame(reader io.Reader) ServerMessageGame {
    result := ServerMessageGame {}
    if ReadBool(reader) {
        var PlayerViewValue PlayerView
        PlayerViewValue = ReadPlayerView(reader)
        result.PlayerView = &PlayerViewValue
    } else {
        result.PlayerView = nil
    }
    return result
}
func (value ServerMessageGame) Write(writer io.Writer) {
    if value.PlayerView == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        (*value.PlayerView).Write(writer)
    }
}
