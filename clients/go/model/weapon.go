package model

import (
	"io"

	mStream "../stream"
)

type Weapon struct {
	Typ          WeaponType
	Params       *WeaponParams
	Magazine     int32
	WasShooting  bool
	Spread       float64
	FireTimer    float64
	LastAngle    float64
	LastFireTick int32
}

func NewWeapon(typ WeaponType, params *WeaponParams, magazine int32, wasShooting bool, spread float64,
	fireTimer float64, lastAngle float64, lastFireTick int32) *Weapon {
	return &Weapon{
		Typ:          typ,
		Params:       params,
		Magazine:     magazine,
		WasShooting:  wasShooting,
		Spread:       spread,
		FireTimer:    fireTimer,
		LastAngle:    lastAngle,
		LastFireTick: lastFireTick,
	}
}
func ReadWeapon(reader io.Reader) *Weapon {
	result := &Weapon{
		Typ:         ReadWeaponType(reader),
		Params:      ReadWeaponParams(reader),
		Magazine:    mStream.ReadInt32(reader),
		WasShooting: mStream.ReadBool(reader),
		Spread:      mStream.ReadFloat64(reader),
	}

	if mStream.ReadBool(reader) {
		result.FireTimer = mStream.ReadFloat64(reader)
	} else {
		result.FireTimer = -1
	}
	if mStream.ReadBool(reader) {
		result.LastAngle = mStream.ReadFloat64(reader)
	} else {
		result.LastAngle = -1
	}
	if mStream.ReadBool(reader) {
		result.LastFireTick = mStream.ReadInt32(reader)
	} else {
		result.LastFireTick = -1
	}
	return result
}
func (value Weapon) Write(writer io.Writer) {
	mStream.WriteInt32(writer, int32(value.Typ))
	value.Params.Write(writer)
	mStream.WriteInt32(writer, value.Magazine)
	mStream.WriteBool(writer, value.WasShooting)
	mStream.WriteFloat64(writer, value.Spread)
	if value.FireTimer == -1 {
		mStream.WriteBool(writer, false)
	} else {
		mStream.WriteBool(writer, true)
		mStream.WriteFloat64(writer, value.FireTimer)
	}
	if value.LastAngle == -1 {
		mStream.WriteBool(writer, false)
	} else {
		mStream.WriteBool(writer, true)
		mStream.WriteFloat64(writer, value.LastAngle)
	}
	if value.LastFireTick == -1 {
		mStream.WriteBool(writer, false)
	} else {
		mStream.WriteBool(writer, true)
		mStream.WriteInt32(writer, value.LastFireTick)
	}
}
