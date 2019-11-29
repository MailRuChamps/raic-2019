package model

import (
	"io"

	mStream "../stream"
)

type JumpState struct {
	CanJump   bool
	Speed     float64
	MaxTime   float64
	CanCancel bool
}

func NewJumpState(canJump bool, speed float64, maxTime float64, canCancel bool) *JumpState {
	return &JumpState{
		CanJump:   canJump,
		Speed:     speed,
		MaxTime:   maxTime,
		CanCancel: canCancel,
	}
}
func ReadJumpState(reader io.Reader) *JumpState {
	return &JumpState{
		CanJump:   mStream.ReadBool(reader),
		Speed:     mStream.ReadFloat64(reader),
		MaxTime:   mStream.ReadFloat64(reader),
		CanCancel: mStream.ReadBool(reader),
	}
}
func (value JumpState) Write(writer io.Writer) {
	mStream.WriteBool(writer, value.CanJump)
	mStream.WriteFloat64(writer, value.Speed)
	mStream.WriteFloat64(writer, value.MaxTime)
	mStream.WriteBool(writer, value.CanCancel)
}
