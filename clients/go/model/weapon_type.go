package model

import (
	mStream "../stream"
	"io"
)

type WeaponType int32

const (
	WeaponTypePistol         WeaponType = 0
	WeaponTypeAssaultRifle   WeaponType = 1
	WeaponTypeRocketLauncher WeaponType = 2
)

func ReadWeaponType(reader io.Reader) WeaponType {
	switch mStream.ReadInt32(reader) {
	case 0:
		return WeaponTypePistol
	case 1:
		return WeaponTypeAssaultRifle
	case 2:
		return WeaponTypeRocketLauncher
	}
	panic("Unexpected discriminant value")
}
