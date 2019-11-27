package model

import "io"
import . "aicup2019/stream"

type PlayerView struct {
    MyId int32
    Game Game
}
func NewPlayerView(myId int32, game Game) PlayerView {
    return PlayerView {
        MyId: myId,
        Game: game,
    }
}
func ReadPlayerView(reader io.Reader) PlayerView {
    result := PlayerView {}
    result.MyId = ReadInt32(reader)
    result.Game = ReadGame(reader)
    return result
}
func (value PlayerView) Write(writer io.Writer) {
    WriteInt32(writer, value.MyId)
    value.Game.Write(writer)
}
