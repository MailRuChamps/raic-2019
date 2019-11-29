package model

/*
	DO NOT CHANGE this module.
	Its automatic replaced on games server
*/

import (
	"fmt"
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
	if radius < 0 {
		panic(fmt.Errorf("NewExplosionParams(): FATAL ERROR radius(%v)<0", radius))
	}
	if damage < 0 {
		panic(fmt.Errorf("NewExplosionParams(): FATAL ERROR damage(%v)<0", damage))
	}
	return &ExplosionParams{
		Radius: radius,
		Damage: damage,
	}
}

//ReadExplosionParams -- read from net ExplosionParams from LocalRunners
func ReadExplosionParams(reader io.Reader) *ExplosionParams {
	radius := mStream.ReadFloat64(reader)
	if radius < 0 {
		panic(fmt.Errorf("ReadExplosionParams(): FATAL ERROR radius(%v)<0", radius))
	}
	damage := mStream.ReadInt32(reader)
	if damage < 0 {
		panic(fmt.Errorf("ReadExplosionParams(): FATAL ERROR damage(%v)<0", damage))
	}
	return &ExplosionParams{
		Radius: radius,
		Damage: damage,
	}
}

//Write -- write ExplosionParams to net to LocalRunner
func (value ExplosionParams) Write(writer io.Writer) {
	mStream.WriteFloat64(writer, value.Radius)
	mStream.WriteInt32(writer, value.Damage)
}
