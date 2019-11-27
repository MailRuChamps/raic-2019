package model

import "io"
import . "aicup2019/stream"

type JumpState struct {
    CanJump bool
    Speed float64
    MaxTime float64
    CanCancel bool
}
func NewJumpState(canJump bool, speed float64, maxTime float64, canCancel bool) JumpState {
    return JumpState {
        CanJump: canJump,
        Speed: speed,
        MaxTime: maxTime,
        CanCancel: canCancel,
    }
}
func ReadJumpState(reader io.Reader) JumpState {
    result := JumpState {}
    result.CanJump = ReadBool(reader)
    result.Speed = ReadFloat64(reader)
    result.MaxTime = ReadFloat64(reader)
    result.CanCancel = ReadBool(reader)
    return result
}
func (value JumpState) Write(writer io.Writer) {
    WriteBool(writer, value.CanJump)
    WriteFloat64(writer, value.Speed)
    WriteFloat64(writer, value.MaxTime)
    WriteBool(writer, value.CanCancel)
}
