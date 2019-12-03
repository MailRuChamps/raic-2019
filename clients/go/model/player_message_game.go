package model

import "io"
import . "aicup2019/stream"

type PlayerMessageGame interface {
    Write(writer io.Writer)
}
func ReadPlayerMessageGame(reader io.Reader) PlayerMessageGame {
    switch ReadInt32(reader) {
        case 0:
            return ReadPlayerMessageGameCustomDataMessage(reader)
        case 1:
            return ReadPlayerMessageGameActionMessage(reader)
    }
    panic("Unexpected discriminant value")
}

type PlayerMessageGameCustomDataMessage struct {
    Data CustomData
}
func NewPlayerMessageGameCustomDataMessage(data CustomData) PlayerMessageGameCustomDataMessage {
    return PlayerMessageGameCustomDataMessage {
        Data: data,
    }
}
func ReadPlayerMessageGameCustomDataMessage(reader io.Reader) PlayerMessageGameCustomDataMessage {
    result := PlayerMessageGameCustomDataMessage {}
    result.Data = ReadCustomData(reader)
    return result
}
func (value PlayerMessageGameCustomDataMessage) Write(writer io.Writer) {
    WriteInt32(writer, 0)
    value.Data.Write(writer)
}

type PlayerMessageGameActionMessage struct {
    Action Versioned
}
func NewPlayerMessageGameActionMessage(action Versioned) PlayerMessageGameActionMessage {
    return PlayerMessageGameActionMessage {
        Action: action,
    }
}
func ReadPlayerMessageGameActionMessage(reader io.Reader) PlayerMessageGameActionMessage {
    result := PlayerMessageGameActionMessage {}
    result.Action = ReadVersioned(reader)
    return result
}
func (value PlayerMessageGameActionMessage) Write(writer io.Writer) {
    WriteInt32(writer, 1)
    value.Action.Write(writer)
}
