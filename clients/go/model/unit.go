package model

import (
	"io"

	mStream "../stream"
)

//Unit -- unit on games field
type Unit struct {
	PlayerId    int32
	Id          int32
	Health      int32
	Position    *Vec2Float64
	Size        *Vec2Float64
	JumpState   *JumpState
	WalkedRight bool
	Stand       bool
	OnGround    bool
	OnLadder    bool
	Mines       int32
	Weapon      *Weapon
}

//NewUnit -- return link to new Unit
func NewUnit(playerId int32, id int32, health int32, position *Vec2Float64, size *Vec2Float64,
	jumpState *JumpState, walkedRight bool, stand bool, onGround bool, onLadder bool,
	mines int32, weapon *Weapon) *Unit {
	return &Unit{
		PlayerId:    playerId,
		Id:          id,
		Health:      health,
		Position:    position,
		Size:        size,
		JumpState:   jumpState,
		WalkedRight: walkedRight,
		Stand:       stand,
		OnGround:    onGround,
		OnLadder:    onLadder,
		Mines:       mines,
		Weapon:      weapon,
	}
}

//ReadUnit -- retrun link to Unit from net connection from LocalRunner
func ReadUnit(reader io.Reader) *Unit {
	result := &Unit{
		PlayerId:    mStream.ReadInt32(reader),
		Id:          mStream.ReadInt32(reader),
		Health:      mStream.ReadInt32(reader),
		Position:    ReadVec2Float64(reader),
		Size:        ReadVec2Float64(reader),
		JumpState:   ReadJumpState(reader),
		WalkedRight: mStream.ReadBool(reader),
		Stand:       mStream.ReadBool(reader),
		OnGround:    mStream.ReadBool(reader),
		OnLadder:    mStream.ReadBool(reader),
		Mines:       mStream.ReadInt32(reader),
	}
	if mStream.ReadBool(reader) {
		result.Weapon = ReadWeapon(reader)
	} else {
		result.Weapon = nil
	}
	return result
}

//Write -- write Unit to net connections to LocalRunner
func (value *Unit) Write(writer io.Writer) {
	mStream.WriteInt32(writer, value.PlayerId)
	mStream.WriteInt32(writer, value.Id)
	mStream.WriteInt32(writer, value.Health)
	value.Position.Write(writer)
	value.Size.Write(writer)
	value.JumpState.Write(writer)
	mStream.WriteBool(writer, value.WalkedRight)
	mStream.WriteBool(writer, value.Stand)
	mStream.WriteBool(writer, value.OnGround)
	mStream.WriteBool(writer, value.OnLadder)
	mStream.WriteInt32(writer, value.Mines)
	if value.Weapon == nil {
		mStream.WriteBool(writer, false)
	} else {
		mStream.WriteBool(writer, true)
		(*value.Weapon).Write(writer)
	}
}
