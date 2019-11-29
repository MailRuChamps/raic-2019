package model

import (
	"io"
	mStream "../stream"
)

type IItem interface {
	Write(writer io.Writer)
}

func ReadItem(reader io.Reader) IItem {
	switch mStream.ReadInt32(reader) {
	case 0:
		return ReadItemHealthPack(reader)
	case 1:
		return ReadItemWeapon(reader)
	case 2:
		return ReadItemMine(reader)
	}
	panic("Unexpected discriminant value")
}

type ItemHealthPack struct {
	Health int32
}

func NewItemHealthPack(health int32) ItemHealthPack {
	return ItemHealthPack{
		Health: health,
	}
}
func ReadItemHealthPack(reader io.Reader) ItemHealthPack {
	result := ItemHealthPack{}
	result.Health = mStream.ReadInt32(reader)
	return result
}
func (value ItemHealthPack) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 0)
	mStream.WriteInt32(writer, value.Health)
}

type ItemWeapon struct {
	WeaponType WeaponType
}

func NewItemWeapon(weaponType WeaponType) ItemWeapon {
	return ItemWeapon{
		WeaponType: weaponType,
	}
}
func ReadItemWeapon(reader io.Reader) ItemWeapon {
	result := ItemWeapon{}
	result.WeaponType = ReadWeaponType(reader)
	return result
}
func (value ItemWeapon) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 1)
	mStream.WriteInt32(writer, int32(value.WeaponType))
}

type ItemMine struct {
}

func NewItemMine() ItemMine {
	return ItemMine{}
}
func ReadItemMine(reader io.Reader) ItemMine {
	result := ItemMine{}
	return result
}
func (value ItemMine) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 2)
}
