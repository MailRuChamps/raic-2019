package model

import (
	"io"

	mStream "../stream"
)

//ExplosionParams -- explose with bullet strict games object
type ExplosionParams struct {
	Radius float64
	Damage int32
}

//NewExplosionParams -- return link to new ExplosionParams
func NewExplosionParams(radius float64, damage int32) *ExplosionParams {
	return &ExplosionParams{
		Radius: radius,
		Damage: damage,
	}
}

//ReadExplosionParams -- read from net ExplosionParams from LocalRunners
func ReadExplosionParams(reader io.Reader) *ExplosionParams {
	return &ExplosionParams{
		Radius: mStream.ReadFloat64(reader),
		Damage: mStream.ReadInt32(reader),
	}
}

//Write -- write ExplosionParams to net to LocalRunner
func (value ExplosionParams) Write(writer io.Writer) {
	mStream.WriteFloat64(writer, value.Radius)
	mStream.WriteInt32(writer, value.Damage)
}
