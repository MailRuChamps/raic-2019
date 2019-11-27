package model

import "io"
import . "aicup2019/stream"

type ExplosionParams struct {
    Radius float64
    Damage int32
}
func NewExplosionParams(radius float64, damage int32) ExplosionParams {
    return ExplosionParams {
        Radius: radius,
        Damage: damage,
    }
}
func ReadExplosionParams(reader io.Reader) ExplosionParams {
    result := ExplosionParams {}
    result.Radius = ReadFloat64(reader)
    result.Damage = ReadInt32(reader)
    return result
}
func (value ExplosionParams) Write(writer io.Writer) {
    WriteFloat64(writer, value.Radius)
    WriteInt32(writer, value.Damage)
}
