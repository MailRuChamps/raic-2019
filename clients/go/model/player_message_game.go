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
    Action map[int32]UnitAction
}
func NewPlayerMessageGameActionMessage(action map[int32]UnitAction) PlayerMessageGameActionMessage {
    return PlayerMessageGameActionMessage {
        Action: action,
    }
}
func ReadPlayerMessageGameActionMessage(reader io.Reader) PlayerMessageGameActionMessage {
    result := PlayerMessageGameActionMessage {}
    ActionSize := ReadInt32(reader)
    result.Action = make(map[int32]UnitAction)
    for i := int32(0); i < ActionSize; i++ {
        var ActionKey int32
        ActionKey = ReadInt32(reader)
        var ActionValue UnitAction
        ActionValue = ReadUnitAction(reader)
        result.Action[ActionKey] = ActionValue
    }
    return result
}
func (value PlayerMessageGameActionMessage) Write(writer io.Writer) {
    WriteInt32(writer, 1)
    WriteInt32(writer, int32(len(value.Action)))
    for ActionKey, ActionValue := range value.Action {
        WriteInt32(writer, ActionKey)
        ActionValue.Write(writer)
    }
}
