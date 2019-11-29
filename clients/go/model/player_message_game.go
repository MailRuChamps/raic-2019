package model

import (
	"io"

	mStream "../stream"
)

type PlayerMessageGame interface {
	Write(writer io.Writer)
}

func ReadPlayerMessageGame(reader io.Reader) PlayerMessageGame {
	switch mStream.ReadInt32(reader) {
	case 0:
		return ReadPlayerMessageGameCustomDataMessage(reader)
	case 1:
		return ReadPlayerMessageGameActionMessage(reader)
	}
	panic("Unexpected discriminant value")
}

type PlayerMessageGameCustomDataMessage struct {
	Data ICustomData
}

func NewPlayerMessageGameCustomDataMessage(data ICustomData) PlayerMessageGameCustomDataMessage {
	return PlayerMessageGameCustomDataMessage{
		Data: data,
	}
}
func ReadPlayerMessageGameCustomDataMessage(reader io.Reader) PlayerMessageGameCustomDataMessage {
	result := PlayerMessageGameCustomDataMessage{}
	result.Data = ReadCustomData(reader)
	return result
}
func (value PlayerMessageGameCustomDataMessage) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 0)
	value.Data.Write(writer)
}

type PlayerMessageGameActionMessage struct {
	Action map[int32]*UnitAction
}

func NewPlayerMessageGameActionMessage(action map[int32]*UnitAction) *PlayerMessageGameActionMessage {
	return &PlayerMessageGameActionMessage{
		Action: action,
	}
}
func ReadPlayerMessageGameActionMessage(reader io.Reader) *PlayerMessageGameActionMessage {
	result := &PlayerMessageGameActionMessage{}
	ActionSize := mStream.ReadInt32(reader)
	result.Action = make(map[int32]*UnitAction)
	for i := int32(0); i < ActionSize; i++ {
		ActionKey := mStream.ReadInt32(reader)
		result.Action[ActionKey] = ReadUnitAction(reader)
	}
	return result
}
func (value PlayerMessageGameActionMessage) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 1)
	mStream.WriteInt32(writer, int32(len(value.Action)))
	for ActionKey, ActionValue := range value.Action {
		mStream.WriteInt32(writer, ActionKey)
		ActionValue.Write(writer)
	}
}
