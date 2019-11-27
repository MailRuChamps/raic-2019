package model

import "io"
import . "aicup2019/stream"

type UnitAction struct {
    Velocity float64
    Jump bool
    JumpDown bool
    Aim Vec2Float64
    Shoot bool
    SwapWeapon bool
    PlantMine bool
}
func NewUnitAction(velocity float64, jump bool, jumpDown bool, aim Vec2Float64, shoot bool, swapWeapon bool, plantMine bool) UnitAction {
    return UnitAction {
        Velocity: velocity,
        Jump: jump,
        JumpDown: jumpDown,
        Aim: aim,
        Shoot: shoot,
        SwapWeapon: swapWeapon,
        PlantMine: plantMine,
    }
}
func ReadUnitAction(reader io.Reader) UnitAction {
    result := UnitAction {}
    result.Velocity = ReadFloat64(reader)
    result.Jump = ReadBool(reader)
    result.JumpDown = ReadBool(reader)
    result.Aim = ReadVec2Float64(reader)
    result.Shoot = ReadBool(reader)
    result.SwapWeapon = ReadBool(reader)
    result.PlantMine = ReadBool(reader)
    return result
}
func (value UnitAction) Write(writer io.Writer) {
    WriteFloat64(writer, value.Velocity)
    WriteBool(writer, value.Jump)
    WriteBool(writer, value.JumpDown)
    value.Aim.Write(writer)
    WriteBool(writer, value.Shoot)
    WriteBool(writer, value.SwapWeapon)
    WriteBool(writer, value.PlantMine)
}
