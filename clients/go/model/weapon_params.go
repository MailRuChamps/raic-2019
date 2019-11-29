package model

import (
	"io"
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
	maxSpread float64, recoil float64, aimSpeed float64, bullet BulletParams, explosion *ExplosionParams) *WeaponParams {
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
		MagazineSize: ReadInt32(reader),
		FireRate:     ReadFloat64(reader),
		ReloadTime:   ReadFloat64(reader),
		MinSpread:    ReadFloat64(reader),
		MaxSpread:    ReadFloat64(reader),
		Recoil:       ReadFloat64(reader),
		AimSpeed:     ReadFloat64(reader),
		Bullet:       ReadBulletParams(reader),
	}
	if ReadBool(reader) {
		ExplosionValue := ReadExplosionParams(reader)
		result.Explosion = &ExplosionValue
	} else {
		result.Explosion = nil
	}
	return result
}
func (value WeaponParams) Write(writer io.Writer) {
	WriteInt32(writer, value.MagazineSize)
	WriteFloat64(writer, value.FireRate)
	WriteFloat64(writer, value.ReloadTime)
	WriteFloat64(writer, value.MinSpread)
	WriteFloat64(writer, value.MaxSpread)
	WriteFloat64(writer, value.Recoil)
	WriteFloat64(writer, value.AimSpeed)
	value.Bullet.Write(writer)
	if value.Explosion == nil {
		WriteBool(writer, false)
	} else {
		WriteBool(writer, true)
		(*value.Explosion).Write(writer)
	}
}
