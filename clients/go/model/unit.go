package model

import "io"
import . "aicup2019/stream"

type Unit struct {
    PlayerId int32
    Id int32
    Health int32
    Position Vec2Float64
    Size Vec2Float64
    JumpState JumpState
    WalkedRight bool
    Stand bool
    OnGround bool
    OnLadder bool
    Mines int32
    Weapon *Weapon
}
func NewUnit(playerId int32, id int32, health int32, position Vec2Float64, size Vec2Float64, jumpState JumpState, walkedRight bool, stand bool, onGround bool, onLadder bool, mines int32, weapon *Weapon) Unit {
    return Unit {
        PlayerId: playerId,
        Id: id,
        Health: health,
        Position: position,
        Size: size,
        JumpState: jumpState,
        WalkedRight: walkedRight,
        Stand: stand,
        OnGround: onGround,
        OnLadder: onLadder,
        Mines: mines,
        Weapon: weapon,
    }
}
func ReadUnit(reader io.Reader) Unit {
    result := Unit {}
    result.PlayerId = ReadInt32(reader)
    result.Id = ReadInt32(reader)
    result.Health = ReadInt32(reader)
    result.Position = ReadVec2Float64(reader)
    result.Size = ReadVec2Float64(reader)
    result.JumpState = ReadJumpState(reader)
    result.WalkedRight = ReadBool(reader)
    result.Stand = ReadBool(reader)
    result.OnGround = ReadBool(reader)
    result.OnLadder = ReadBool(reader)
    result.Mines = ReadInt32(reader)
    if ReadBool(reader) {
        var WeaponValue Weapon
        WeaponValue = ReadWeapon(reader)
        result.Weapon = &WeaponValue
    } else {
        result.Weapon = nil
    }
    return result
}
func (value Unit) Write(writer io.Writer) {
    WriteInt32(writer, value.PlayerId)
    WriteInt32(writer, value.Id)
    WriteInt32(writer, value.Health)
    value.Position.Write(writer)
    value.Size.Write(writer)
    value.JumpState.Write(writer)
    WriteBool(writer, value.WalkedRight)
    WriteBool(writer, value.Stand)
    WriteBool(writer, value.OnGround)
    WriteBool(writer, value.OnLadder)
    WriteInt32(writer, value.Mines)
    if value.Weapon == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        (*value.Weapon).Write(writer)
    }
}
