package model

import "io"
import . "aicup2019/stream"

type Weapon struct {
    Typ WeaponType
    Params WeaponParams
    Magazine int32
    WasShooting bool
    Spread float64
    FireTimer *float64
    LastAngle *float64
    LastFireTick *int32
}
func NewWeapon(typ WeaponType, params WeaponParams, magazine int32, wasShooting bool, spread float64, fireTimer *float64, lastAngle *float64, lastFireTick *int32) Weapon {
    return Weapon {
        Typ: typ,
        Params: params,
        Magazine: magazine,
        WasShooting: wasShooting,
        Spread: spread,
        FireTimer: fireTimer,
        LastAngle: lastAngle,
        LastFireTick: lastFireTick,
    }
}
func ReadWeapon(reader io.Reader) Weapon {
    result := Weapon {}
    result.Typ = ReadWeaponType(reader)
    result.Params = ReadWeaponParams(reader)
    result.Magazine = ReadInt32(reader)
    result.WasShooting = ReadBool(reader)
    result.Spread = ReadFloat64(reader)
    if ReadBool(reader) {
        var FireTimerValue float64
        FireTimerValue = ReadFloat64(reader)
        result.FireTimer = &FireTimerValue
    } else {
        result.FireTimer = nil
    }
    if ReadBool(reader) {
        var LastAngleValue float64
        LastAngleValue = ReadFloat64(reader)
        result.LastAngle = &LastAngleValue
    } else {
        result.LastAngle = nil
    }
    if ReadBool(reader) {
        var LastFireTickValue int32
        LastFireTickValue = ReadInt32(reader)
        result.LastFireTick = &LastFireTickValue
    } else {
        result.LastFireTick = nil
    }
    return result
}
func (value Weapon) Write(writer io.Writer) {
    WriteInt32(writer, int32(value.Typ))
    value.Params.Write(writer)
    WriteInt32(writer, value.Magazine)
    WriteBool(writer, value.WasShooting)
    WriteFloat64(writer, value.Spread)
    if value.FireTimer == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        WriteFloat64(writer, (*value.FireTimer))
    }
    if value.LastAngle == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        WriteFloat64(writer, (*value.LastAngle))
    }
    if value.LastFireTick == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        WriteInt32(writer, (*value.LastFireTick))
    }
}
