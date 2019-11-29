package model

import (
	"io"

	mStream "../stream"
)

//Bullet -- type for model bullets of weapons
type Bullet struct {
	WeaponType      WeaponType
	UnitId          int32
	PlayerId        int32
	Position        *Vec2Float64
	Velocity        *Vec2Float64
	Damage          int32
	Size            float64
	ExplosionParams *ExplosionParams
}

//NewBullet -- return new bullet of weapons for shooting
func NewBullet(weaponType WeaponType,
	unitID int32, playerID int32, position *Vec2Float64, velocity *Vec2Float64,
	damage int32, size float64, explosionParams *ExplosionParams) *Bullet {
	return &Bullet{
		WeaponType:      weaponType,
		UnitId:          unitID,
		PlayerId:        playerID,
		Position:        position,
		Velocity:        velocity,
		Damage:          damage,
		Size:            size,
		ExplosionParams: explosionParams,
	}
}

//ReadBullet -- get bullet from net stream from LocalRunner
func ReadBullet(reader io.Reader) *Bullet {
	result := &Bullet{
		WeaponType: ReadWeaponType(reader),
		UnitId:     mStream.ReadInt32(reader),
		PlayerId:   mStream.ReadInt32(reader),
		Position:   ReadVec2Float64(reader),
		Velocity:   ReadVec2Float64(reader),
		Damage:     mStream.ReadInt32(reader),
		Size:       mStream.ReadFloat64(reader),
	}

	if mStream.ReadBool(reader) {
		ExplosionParamsValue := ReadExplosionParams(reader)
		result.ExplosionParams = ExplosionParamsValue
	} else {
		result.ExplosionParams = nil
	}
	return result
}

//Write -- write bullet to net stream to LocalRunner
func (value *Bullet) Write(writer io.Writer) {
	mStream.WriteInt32(writer, int32(value.WeaponType))
	mStream.WriteInt32(writer, value.UnitId)
	mStream.WriteInt32(writer, value.PlayerId)
	value.Position.Write(writer)
	value.Velocity.Write(writer)
	mStream.WriteInt32(writer, value.Damage)
	mStream.WriteFloat64(writer, value.Size)
	if value.ExplosionParams == nil {
		mStream.WriteBool(writer, false)
	} else {
		mStream.WriteBool(writer, true)
		(*value.ExplosionParams).Write(writer)
	}
}
