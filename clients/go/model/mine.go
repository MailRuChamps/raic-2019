package model

import "io"
import . "aicup2019/stream"

type Mine struct {
    PlayerId int32
    Position Vec2Float64
    Size Vec2Float64
    State MineState
    Timer *float64
    TriggerRadius float64
    ExplosionParams ExplosionParams
}
func NewMine(playerId int32, position Vec2Float64, size Vec2Float64, state MineState, timer *float64, triggerRadius float64, explosionParams ExplosionParams) Mine {
    return Mine {
        PlayerId: playerId,
        Position: position,
        Size: size,
        State: state,
        Timer: timer,
        TriggerRadius: triggerRadius,
        ExplosionParams: explosionParams,
    }
}
func ReadMine(reader io.Reader) Mine {
    result := Mine {}
    result.PlayerId = ReadInt32(reader)
    result.Position = ReadVec2Float64(reader)
    result.Size = ReadVec2Float64(reader)
    result.State = ReadMineState(reader)
    if ReadBool(reader) {
        var TimerValue float64
        TimerValue = ReadFloat64(reader)
        result.Timer = &TimerValue
    } else {
        result.Timer = nil
    }
    result.TriggerRadius = ReadFloat64(reader)
    result.ExplosionParams = ReadExplosionParams(reader)
    return result
}
func (value Mine) Write(writer io.Writer) {
    WriteInt32(writer, value.PlayerId)
    value.Position.Write(writer)
    value.Size.Write(writer)
    WriteInt32(writer, int32(value.State))
    if value.Timer == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        WriteFloat64(writer, (*value.Timer))
    }
    WriteFloat64(writer, value.TriggerRadius)
    value.ExplosionParams.Write(writer)
}
