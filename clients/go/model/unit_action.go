package model

import (
	"io"

	mStream "../stream"
)

type UnitAction struct {
	Velocity   float64
	Jump       bool
	JumpDown   bool
	Aim        *Vec2Float64
	Shoot      bool
	SwapWeapon bool
	PlantMine  bool
}

func NewUnitAction(velocity float64, jump bool, jumpDown bool, aim *Vec2Float64, shoot bool, swapWeapon bool, plantMine bool) *UnitAction {
	return &UnitAction{
		Velocity:   velocity,
		Jump:       jump,
		JumpDown:   jumpDown,
		Aim:        aim,
		Shoot:      shoot,
		SwapWeapon: swapWeapon,
		PlantMine:  plantMine,
	}
}
func ReadUnitAction(reader io.Reader) *UnitAction {
	return &UnitAction{
		Velocity:   mStream.ReadFloat64(reader),
		Jump:       mStream.ReadBool(reader),
		JumpDown:   mStream.ReadBool(reader),
		Aim:        ReadVec2Float64(reader),
		Shoot:      mStream.ReadBool(reader),
		SwapWeapon: mStream.ReadBool(reader),
		PlantMine:  mStream.ReadBool(reader),
	}
}
func (value UnitAction) Write(writer io.Writer) {
	mStream.WriteFloat64(writer, value.Velocity)
	mStream.WriteBool(writer, value.Jump)
	mStream.WriteBool(writer, value.JumpDown)
	value.Aim.Write(writer)
	mStream.WriteBool(writer, value.Shoot)
	mStream.WriteBool(writer, value.SwapWeapon)
	mStream.WriteBool(writer, value.PlantMine)
}
