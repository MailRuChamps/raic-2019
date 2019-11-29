package model

import (
	"io"
)

//BulletParams -- params for all bullets of andere weapons
type BulletParams struct {
	Speed  float64
	Size   float64
	Damage int32
}

//NewBulletParams -- return link to new BulletParams
func NewBulletParams(speed float64, size float64, damage int32) *BulletParams {
	return &BulletParams{
		Speed:  speed,
		Size:   size,
		Damage: damage,
	}
}

//ReadBulletParams -- read BulletParams from net connection from LocalRunner
func ReadBulletParams(reader io.Reader) *BulletParams {
	result := &BulletParams{
		Speed:  ReadFloat64(reader),
		Size:   ReadFloat64(reader),
		Damage: ReadInt32(reader),
	}
	return result
}

//Write -- write BulletParams to net connection to LocalRunner
func (value *BulletParams) Write(writer io.Writer) {
	WriteFloat64(writer, value.Speed)
	WriteFloat64(writer, value.Size)
	WriteInt32(writer, value.Damage)
}
