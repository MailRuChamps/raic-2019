package model

/*
	DO NOT CHANGE this module.
	Its automatic replaced on games server
*/

import (
	"fmt"
	"io"

	mStream "../stream"
)

type IItem interface {
	Write(writer io.Writer)
}

func ReadItem(reader io.Reader) (item IItem) {
	switch mStream.ReadInt32(reader) {
	case 0:
		item = ReadItemHealthPack(reader)
	case 1:
		item = ReadItemWeapon(reader)
	case 2:
		item = ReadItemMine(reader)
	default:
		panic("Unexpected discriminant value")
	}
	if item == nil {
		panic(fmt.Errorf("ReadItem(): FATAL ERROR item==nil"))
	}
	return item
}

type ItemHealthPack struct {
	Health int32
}

func NewItemHealthPack(health int32) *ItemHealthPack {
	if health < 0 {
		panic(fmt.Errorf("NewItemHealthPack(): FATAL ERROR health(%v)<0", health))
	}
	return &ItemHealthPack{
		Health: health,
	}
}

func ReadItemHealthPack(reader io.Reader) *ItemHealthPack {
	healt := mStream.ReadInt32(reader)
	if healt < 0 {
		panic(fmt.Errorf("ReadItemHealthPack(): FATAL ERROR healt(%v)<0", healt))
	}
	return &ItemHealthPack{
		Health: healt,
	}
}

func (value ItemHealthPack) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 0)
	mStream.WriteInt32(writer, value.Health)
}

type ItemWeapon struct {
	WeaponType WeaponType
}

func NewItemWeapon(weaponType WeaponType) *ItemWeapon {
	if !(WeaponTypePistol <= weaponType && weaponType <= WeaponTypeRocketLauncher) {
		panic(fmt.Errorf("NewItemWeapon(): FATAL ERROR 0<=weaponType(%v)<=2", weaponType))
	}
	return &ItemWeapon{
		WeaponType: weaponType,
	}
}

func ReadItemWeapon(reader io.Reader) *ItemWeapon {
	weap := ReadWeaponType(reader)
	if !(WeaponTypePistol <= weap && weap <= WeaponTypeRocketLauncher) {
		panic(fmt.Errorf("ReadItemWeapon(): FATAL ERROR 0<=weap(%v)<=2", weap))
	}
	return &ItemWeapon{
		WeaponType: weap,
	}
}

func (value ItemWeapon) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 1)
	mStream.WriteInt32(writer, int32(value.WeaponType))
}

type ItemMine struct {
}

func NewItemMine() *ItemMine {
	return &ItemMine{}
}
func ReadItemMine(reader io.Reader) *ItemMine {
	return &ItemMine{}
}
func (value ItemMine) Write(writer io.Writer) {
	mStream.WriteInt32(writer, 2)
}
