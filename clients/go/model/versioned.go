package model

import "io"
import . "aicup2019/stream"

type Versioned struct {
    Inner map[int32]UnitAction
}
func NewVersioned(inner map[int32]UnitAction) Versioned {
    return Versioned {
        Inner: inner,
    }
}
func ReadVersioned(reader io.Reader) Versioned {
    result := Versioned {}
    InnerSize := ReadInt32(reader)
    result.Inner = make(map[int32]UnitAction)
    for i := int32(0); i < InnerSize; i++ {
        var InnerKey int32
        InnerKey = ReadInt32(reader)
        var InnerValue UnitAction
        InnerValue = ReadUnitAction(reader)
        result.Inner[InnerKey] = InnerValue
    }
    return result
}
func (value Versioned) Write(writer io.Writer) {
    WriteInt32(writer, 43981)
    WriteInt32(writer, int32(len(value.Inner)))
    for InnerKey, InnerValue := range value.Inner {
        WriteInt32(writer, InnerKey)
        InnerValue.Write(writer)
    }
}
