package model

import (
	"io"

	mStream "../stream"
)

type Properties struct {
	MaxTickCount           int32
	TeamSize               int32
	TicksPerSecond         float64
	UpdatesPerTick         int32
	LootBoxSize            *Vec2Float64
	UnitSize               *Vec2Float64
	UnitMaxHorizontalSpeed float64
	UnitFallSpeed          float64
	UnitJumpTime           float64
	UnitJumpSpeed          float64
	JumpPadJumpTime        float64
	JumpPadJumpSpeed       float64
	UnitMaxHealth          int32
	HealthPackHealth       int32
	WeaponParams           map[WeaponType]*WeaponParams
	MineSize               *Vec2Float64
	MineExplosionParams    *ExplosionParams
	MinePrepareTime        float64
	MineTriggerTime        float64
	MineTriggerRadius      float64
	KillScore              int32
}

func NewProperties(maxTickCount int32, teamSize int32, ticksPerSecond float64,
	updatesPerTick int32, lootBoxSize *Vec2Float64, unitSize *Vec2Float64, unitMaxHorizontalSpeed float64,
	unitFallSpeed float64, unitJumpTime float64, unitJumpSpeed float64, jumpPadJumpTime float64,
	jumpPadJumpSpeed float64, unitMaxHealth int32, healthPackHealth int32,
	weaponParams map[WeaponType]*WeaponParams, mineSize *Vec2Float64, mineExplosionParams *ExplosionParams,
	minePrepareTime float64, mineTriggerTime float64, mineTriggerRadius float64, killScore int32) *Properties {
	return &Properties{
		MaxTickCount:           maxTickCount,
		TeamSize:               teamSize,
		TicksPerSecond:         ticksPerSecond,
		UpdatesPerTick:         updatesPerTick,
		LootBoxSize:            lootBoxSize,
		UnitSize:               unitSize,
		UnitMaxHorizontalSpeed: unitMaxHorizontalSpeed,
		UnitFallSpeed:          unitFallSpeed,
		UnitJumpTime:           unitJumpTime,
		UnitJumpSpeed:          unitJumpSpeed,
		JumpPadJumpTime:        jumpPadJumpTime,
		JumpPadJumpSpeed:       jumpPadJumpSpeed,
		UnitMaxHealth:          unitMaxHealth,
		HealthPackHealth:       healthPackHealth,
		WeaponParams:           weaponParams,
		MineSize:               mineSize,
		MineExplosionParams:    mineExplosionParams,
		MinePrepareTime:        minePrepareTime,
		MineTriggerTime:        mineTriggerTime,
		MineTriggerRadius:      mineTriggerRadius,
		KillScore:              killScore,
	}
}

//ReadProperties -- read from net Properties from LocalRunner
func ReadProperties(reader io.Reader) *Properties {
	result := &Properties{
		MaxTickCount:           mStream.ReadInt32(reader),
		TeamSize:               mStream.ReadInt32(reader),
		TicksPerSecond:         mStream.ReadFloat64(reader),
		UpdatesPerTick:         mStream.ReadInt32(reader),
		LootBoxSize:            ReadVec2Float64(reader),
		UnitSize:               ReadVec2Float64(reader),
		UnitMaxHorizontalSpeed: mStream.ReadFloat64(reader),
		UnitFallSpeed:          mStream.ReadFloat64(reader),
		UnitJumpTime:           mStream.ReadFloat64(reader),
		UnitJumpSpeed:          mStream.ReadFloat64(reader),
		JumpPadJumpTime:        mStream.ReadFloat64(reader),
		JumpPadJumpSpeed:       mStream.ReadFloat64(reader),
		UnitMaxHealth:          mStream.ReadInt32(reader),
		HealthPackHealth:       mStream.ReadInt32(reader),
		MineSize:               ReadVec2Float64(reader),
		MineExplosionParams:    ReadExplosionParams(reader),
		MinePrepareTime:        mStream.ReadFloat64(reader),
		MineTriggerTime:        mStream.ReadFloat64(reader),
		MineTriggerRadius:      mStream.ReadFloat64(reader),
		KillScore:              mStream.ReadInt32(reader),
	}
	WeaponParamsSize := mStream.ReadInt32(reader)
	result.WeaponParams = make(map[WeaponType]*WeaponParams)
	for i := int32(0); i < WeaponParamsSize; i++ {
		WeaponParamsKey := ReadWeaponType(reader)
		WeaponParamsValue := ReadWeaponParams(reader)
		result.WeaponParams[WeaponParamsKey] = WeaponParamsValue
	}
	return result
}
func (value Properties) Write(writer io.Writer) {
	mStream.WriteInt32(writer, value.MaxTickCount)
	mStream.WriteInt32(writer, value.TeamSize)
	mStream.WriteFloat64(writer, value.TicksPerSecond)
	mStream.WriteInt32(writer, value.UpdatesPerTick)
	value.LootBoxSize.Write(writer)
	value.UnitSize.Write(writer)
	mStream.WriteFloat64(writer, value.UnitMaxHorizontalSpeed)
	mStream.WriteFloat64(writer, value.UnitFallSpeed)
	mStream.WriteFloat64(writer, value.UnitJumpTime)
	mStream.WriteFloat64(writer, value.UnitJumpSpeed)
	mStream.WriteFloat64(writer, value.JumpPadJumpTime)
	mStream.WriteFloat64(writer, value.JumpPadJumpSpeed)
	mStream.WriteInt32(writer, value.UnitMaxHealth)
	mStream.WriteInt32(writer, value.HealthPackHealth)
	mStream.WriteInt32(writer, int32(len(value.WeaponParams)))
	for WeaponParamsKey, WeaponParamsValue := range value.WeaponParams {
		mStream.WriteInt32(writer, int32(WeaponParamsKey))
		WeaponParamsValue.Write(writer)
	}
	value.MineSize.Write(writer)
	value.MineExplosionParams.Write(writer)
	mStream.WriteFloat64(writer, value.MinePrepareTime)
	mStream.WriteFloat64(writer, value.MineTriggerTime)
	mStream.WriteFloat64(writer, value.MineTriggerRadius)
	mStream.WriteInt32(writer, value.KillScore)
}
