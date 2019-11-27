package model

import "io"
import . "aicup2019/stream"

type BulletParams struct {
    Speed float64
    Size float64
    Damage int32
}
func NewBulletParams(speed float64, size float64, damage int32) BulletParams {
    return BulletParams {
        Speed: speed,
        Size: size,
        Damage: damage,
    }
}
func ReadBulletParams(reader io.Reader) BulletParams {
    result := BulletParams {}
    result.Speed = ReadFloat64(reader)
    result.Size = ReadFloat64(reader)
    result.Damage = ReadInt32(reader)
    return result
}
func (value BulletParams) Write(writer io.Writer) {
    WriteFloat64(writer, value.Speed)
    WriteFloat64(writer, value.Size)
    WriteInt32(writer, value.Damage)
}
