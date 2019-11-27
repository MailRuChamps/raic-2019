package model

import "io"
import . "aicup2019/stream"

type Player struct {
    Id int32
    Score int32
}
func NewPlayer(id int32, score int32) Player {
    return Player {
        Id: id,
        Score: score,
    }
}
func ReadPlayer(reader io.Reader) Player {
    result := Player {}
    result.Id = ReadInt32(reader)
    result.Score = ReadInt32(reader)
    return result
}
func (value Player) Write(writer io.Writer) {
    WriteInt32(writer, value.Id)
    WriteInt32(writer, value.Score)
}
