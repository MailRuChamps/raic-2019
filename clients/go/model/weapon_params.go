package model

import (
	"io"

	mStream "../stream"
)

type WeaponParams struct {
	MagazineSize int32
	FireRate     float64
	ReloadTime   float64
	MinSpread    float64
	MaxSpread    float64
	Recoil       float64
	AimSpeed     float64
	Bullet       *BulletParams
	Explosion    *ExplosionParams
}

func NewWeaponParams(magazineSize int32, fireRate float64, reloadTime float64, minSpread float64,
	maxSpread float64, recoil float64, aimSpeed float64, bullet *BulletParams, explosion *ExplosionParams) *WeaponParams {
	return &WeaponParams{
		MagazineSize: magazineSize,
		FireRate:     fireRate,
		ReloadTime:   reloadTime,
		MinSpread:    minSpread,
		MaxSpread:    maxSpread,
		Recoil:       recoil,
		AimSpeed:     aimSpeed,
		Bullet:       bullet,
		Explosion:    explosion,
	}
}
func ReadWeaponParams(reader io.Reader) *WeaponParams {
	result := &WeaponParams{
		MagazineSize: mStream.ReadInt32(reader),
		FireRate:     mStream.ReadFloat64(reader),
		ReloadTime:   mStream.ReadFloat64(reader),
		MinSpread:    mStream.ReadFloat64(reader),
		MaxSpread:    mStream.ReadFloat64(reader),
		Recoil:       mStream.ReadFloat64(reader),
		AimSpeed:     mStream.ReadFloat64(reader),
		Bullet:       ReadBulletParams(reader),
	}
	if mStream.ReadBool(reader) {
		result.Explosion = ReadExplosionParams(reader)
	} else {
		result.Explosion = nil
	}
	return result
}
func (value WeaponParams) Write(writer io.Writer) {
	mStream.WriteInt32(writer, value.MagazineSize)
	mStream.WriteFloat64(writer, value.FireRate)
	mStream.WriteFloat64(writer, value.ReloadTime)
	mStream.WriteFloat64(writer, value.MinSpread)
	mStream.WriteFloat64(writer, value.MaxSpread)
	mStream.WriteFloat64(writer, value.Recoil)
	mStream.WriteFloat64(writer, value.AimSpeed)
	value.Bullet.Write(writer)
	if value.Explosion == nil {
		mStream.WriteBool(writer, false)
	} else {
		mStream.WriteBool(writer, true)
		(*value.Explosion).Write(writer)
	}
}
