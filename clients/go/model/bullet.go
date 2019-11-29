package model

/*
	DO NOT CHANGE this module.
	Its automatic replaced on games server
*/

import (
	"io"

	"fmt"

	mStream "../stream"
)

//Bullet -- type for model bullets of weapons
type Bullet struct {
	WeaponType      WeaponType
	UnitID          int32
	PlayerID        int32
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
	if !(WeaponTypePistol <= weaponType && weaponType <= WeaponTypeRocketLauncher) {
		panic(fmt.Errorf("NewBullet(): FATAL ERROR weaponType(%v) invalid", weaponType))
	}
	if position == nil {
		panic(fmt.Errorf("NewBullet(): FATAL ERROR position==nil"))
	}
	if velocity == nil {
		panic(fmt.Errorf("NewBullet(): FATAL ERROR velocity==nil"))
	}
	if damage < 0 {
		panic(fmt.Errorf("NewBullet(): FATAL ERROR damage(%v)<0", damage))
	}
	if size < 0 {
		panic(fmt.Errorf("NewBullet(): FATAL ERROR size(%v)<0", size))
	}
	if explosionParams == nil {
		panic(fmt.Errorf("NewBullet(): FATAL ERROR explosionParams==nil"))
	}
	return &Bullet{
		WeaponType:      weaponType,
		UnitID:          unitID,
		PlayerID:        playerID,
		Position:        position,
		Velocity:        velocity,
		Damage:          damage,
		Size:            size,
		ExplosionParams: explosionParams,
	}
}

//ReadBullet -- get bullet from net stream from LocalRunner
func ReadBullet(reader io.Reader) *Bullet {
	pos := ReadVec2Float64(reader)
	if pos == nil {
		panic(fmt.Errorf("ReadBullet(): FATAL ERROR pos==nil"))
	}
	speed := ReadVec2Float64(reader)
	if speed == nil {
		panic(fmt.Errorf("ReadBullet(): FATAL ERROR speed==nil"))
	}
	damage := mStream.ReadInt32(reader)
	if damage < 0 {
		panic(fmt.Errorf("ReadBullet(): FATAL ERROR damage(%v)<0", damage))
	}
	size := mStream.ReadFloat64(reader)
	if size < 0 {
		panic(fmt.Errorf("ReadBullet(): FATAL ERROR size(%v)<0", size))
	}
	result := &Bullet{
		WeaponType: ReadWeaponType(reader),
		UnitID:     mStream.ReadInt32(reader),
		PlayerID:   mStream.ReadInt32(reader),
		Position:   pos,
		Velocity:   speed,
		Damage:     damage,
		Size:       size,
	}

	if mStream.ReadBool(reader) {
		result.ExplosionParams = ReadExplosionParams(reader)
	} else {
		result.ExplosionParams = nil
	}
	return result
}

//Write -- write bullet to net stream to LocalRunner
func (value *Bullet) Write(writer io.Writer) {
	mStream.WriteInt32(writer, int32(value.WeaponType))
	mStream.WriteInt32(writer, value.UnitID)
	mStream.WriteInt32(writer, value.PlayerID)
	value.Position.Write(writer)
	value.Velocity.Write(writer)
	mStream.WriteInt32(writer, value.Damage)
	mStream.WriteFloat64(writer, value.Size)
	if value.ExplosionParams == nil { //TODO: this not true!!! ExploseParams NEVER nil
		mStream.WriteBool(writer, false)
		return
	}
	mStream.WriteBool(writer, true)
	value.ExplosionParams.Write(writer)
}
