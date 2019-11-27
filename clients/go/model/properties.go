package model

import "io"
import . "aicup2019/stream"

type Properties struct {
    MaxTickCount int32
    TeamSize int32
    TicksPerSecond float64
    UpdatesPerTick int32
    LootBoxSize Vec2Float64
    UnitSize Vec2Float64
    UnitMaxHorizontalSpeed float64
    UnitFallSpeed float64
    UnitJumpTime float64
    UnitJumpSpeed float64
    JumpPadJumpTime float64
    JumpPadJumpSpeed float64
    UnitMaxHealth int32
    HealthPackHealth int32
    WeaponParams map[WeaponType]WeaponParams
    MineSize Vec2Float64
    MineExplosionParams ExplosionParams
    MinePrepareTime float64
    MineTriggerTime float64
    MineTriggerRadius float64
    KillScore int32
}
func NewProperties(maxTickCount int32, teamSize int32, ticksPerSecond float64, updatesPerTick int32, lootBoxSize Vec2Float64, unitSize Vec2Float64, unitMaxHorizontalSpeed float64, unitFallSpeed float64, unitJumpTime float64, unitJumpSpeed float64, jumpPadJumpTime float64, jumpPadJumpSpeed float64, unitMaxHealth int32, healthPackHealth int32, weaponParams map[WeaponType]WeaponParams, mineSize Vec2Float64, mineExplosionParams ExplosionParams, minePrepareTime float64, mineTriggerTime float64, mineTriggerRadius float64, killScore int32) Properties {
    return Properties {
        MaxTickCount: maxTickCount,
        TeamSize: teamSize,
        TicksPerSecond: ticksPerSecond,
        UpdatesPerTick: updatesPerTick,
        LootBoxSize: lootBoxSize,
        UnitSize: unitSize,
        UnitMaxHorizontalSpeed: unitMaxHorizontalSpeed,
        UnitFallSpeed: unitFallSpeed,
        UnitJumpTime: unitJumpTime,
        UnitJumpSpeed: unitJumpSpeed,
        JumpPadJumpTime: jumpPadJumpTime,
        JumpPadJumpSpeed: jumpPadJumpSpeed,
        UnitMaxHealth: unitMaxHealth,
        HealthPackHealth: healthPackHealth,
        WeaponParams: weaponParams,
        MineSize: mineSize,
        MineExplosionParams: mineExplosionParams,
        MinePrepareTime: minePrepareTime,
        MineTriggerTime: mineTriggerTime,
        MineTriggerRadius: mineTriggerRadius,
        KillScore: killScore,
    }
}
func ReadProperties(reader io.Reader) Properties {
    result := Properties {}
    result.MaxTickCount = ReadInt32(reader)
    result.TeamSize = ReadInt32(reader)
    result.TicksPerSecond = ReadFloat64(reader)
    result.UpdatesPerTick = ReadInt32(reader)
    result.LootBoxSize = ReadVec2Float64(reader)
    result.UnitSize = ReadVec2Float64(reader)
    result.UnitMaxHorizontalSpeed = ReadFloat64(reader)
    result.UnitFallSpeed = ReadFloat64(reader)
    result.UnitJumpTime = ReadFloat64(reader)
    result.UnitJumpSpeed = ReadFloat64(reader)
    result.JumpPadJumpTime = ReadFloat64(reader)
    result.JumpPadJumpSpeed = ReadFloat64(reader)
    result.UnitMaxHealth = ReadInt32(reader)
    result.HealthPackHealth = ReadInt32(reader)
    WeaponParamsSize := ReadInt32(reader)
    result.WeaponParams = make(map[WeaponType]WeaponParams)
    for i := int32(0); i < WeaponParamsSize; i++ {
        var WeaponParamsKey WeaponType
        WeaponParamsKey = ReadWeaponType(reader)
        var WeaponParamsValue WeaponParams
        WeaponParamsValue = ReadWeaponParams(reader)
        result.WeaponParams[WeaponParamsKey] = WeaponParamsValue
    }
    result.MineSize = ReadVec2Float64(reader)
    result.MineExplosionParams = ReadExplosionParams(reader)
    result.MinePrepareTime = ReadFloat64(reader)
    result.MineTriggerTime = ReadFloat64(reader)
    result.MineTriggerRadius = ReadFloat64(reader)
    result.KillScore = ReadInt32(reader)
    return result
}
func (value Properties) Write(writer io.Writer) {
    WriteInt32(writer, value.MaxTickCount)
    WriteInt32(writer, value.TeamSize)
    WriteFloat64(writer, value.TicksPerSecond)
    WriteInt32(writer, value.UpdatesPerTick)
    value.LootBoxSize.Write(writer)
    value.UnitSize.Write(writer)
    WriteFloat64(writer, value.UnitMaxHorizontalSpeed)
    WriteFloat64(writer, value.UnitFallSpeed)
    WriteFloat64(writer, value.UnitJumpTime)
    WriteFloat64(writer, value.UnitJumpSpeed)
    WriteFloat64(writer, value.JumpPadJumpTime)
    WriteFloat64(writer, value.JumpPadJumpSpeed)
    WriteInt32(writer, value.UnitMaxHealth)
    WriteInt32(writer, value.HealthPackHealth)
    WriteInt32(writer, int32(len(value.WeaponParams)))
    for WeaponParamsKey, WeaponParamsValue := range value.WeaponParams {
        WriteInt32(writer, int32(WeaponParamsKey))
        WeaponParamsValue.Write(writer)
    }
    value.MineSize.Write(writer)
    value.MineExplosionParams.Write(writer)
    WriteFloat64(writer, value.MinePrepareTime)
    WriteFloat64(writer, value.MineTriggerTime)
    WriteFloat64(writer, value.MineTriggerRadius)
    WriteInt32(writer, value.KillScore)
}
