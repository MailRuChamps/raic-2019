package model

import "io"
import . "aicup2019/stream"

type Bullet struct {
    WeaponType WeaponType
    UnitId int32
    PlayerId int32
    Position Vec2Float64
    Velocity Vec2Float64
    Damage int32
    Size float64
    ExplosionParams *ExplosionParams
}
func NewBullet(weaponType WeaponType, unitId int32, playerId int32, position Vec2Float64, velocity Vec2Float64, damage int32, size float64, explosionParams *ExplosionParams) Bullet {
    return Bullet {
        WeaponType: weaponType,
        UnitId: unitId,
        PlayerId: playerId,
        Position: position,
        Velocity: velocity,
        Damage: damage,
        Size: size,
        ExplosionParams: explosionParams,
    }
}
func ReadBullet(reader io.Reader) Bullet {
    result := Bullet {}
    result.WeaponType = ReadWeaponType(reader)
    result.UnitId = ReadInt32(reader)
    result.PlayerId = ReadInt32(reader)
    result.Position = ReadVec2Float64(reader)
    result.Velocity = ReadVec2Float64(reader)
    result.Damage = ReadInt32(reader)
    result.Size = ReadFloat64(reader)
    if ReadBool(reader) {
        var ExplosionParamsValue ExplosionParams
        ExplosionParamsValue = ReadExplosionParams(reader)
        result.ExplosionParams = &ExplosionParamsValue
    } else {
        result.ExplosionParams = nil
    }
    return result
}
func (value Bullet) Write(writer io.Writer) {
    WriteInt32(writer, int32(value.WeaponType))
    WriteInt32(writer, value.UnitId)
    WriteInt32(writer, value.PlayerId)
    value.Position.Write(writer)
    value.Velocity.Write(writer)
    WriteInt32(writer, value.Damage)
    WriteFloat64(writer, value.Size)
    if value.ExplosionParams == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        (*value.ExplosionParams).Write(writer)
    }
}
