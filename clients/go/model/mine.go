package model

import (
	"io"

	mStream "../stream"
)

type Mine struct {
	PlayerId        int32
	Position        *Vec2Float64
	Size            *Vec2Float64
	State           MineState
	Timer           float64
	TriggerRadius   float64
	ExplosionParams *ExplosionParams
}

func NewMine(playerId int32, position *Vec2Float64, size *Vec2Float64, state MineState, timer float64,
	triggerRadius float64, explosionParams *ExplosionParams) *Mine {
	return &Mine{
		PlayerId:        playerId,
		Position:        position,
		Size:            size,
		State:           state,
		Timer:           timer,
		TriggerRadius:   triggerRadius,
		ExplosionParams: explosionParams,
	}
}
func ReadMine(reader io.Reader) *Mine {
	result := &Mine{
		PlayerId:        mStream.ReadInt32(reader),
		Position:        ReadVec2Float64(reader),
		Size:            ReadVec2Float64(reader),
		State:           ReadMineState(reader),
		TriggerRadius:   mStream.ReadFloat64(reader),
		ExplosionParams: ReadExplosionParams(reader),
	}

	if mStream.ReadBool(reader) {
		TimerValue := mStream.ReadFloat64(reader)
		result.Timer = TimerValue
	} else {
		result.Timer = -1
	}
	return result
}
func (value Mine) Write(writer io.Writer) {
	mStream.WriteInt32(writer, value.PlayerId)
	value.Position.Write(writer)
	value.Size.Write(writer)
	mStream.WriteInt32(writer, int32(value.State))
	if value.Timer < 0 {
		mStream.WriteBool(writer, false)
	} else {
		mStream.WriteBool(writer, true)
		mStream.WriteFloat64(writer, value.Timer)
	}
	mStream.WriteFloat64(writer, value.TriggerRadius)
	value.ExplosionParams.Write(writer)
}
