package model

/*
	DO NOT CHANGE this module.
	Its automatic replaced on games server
*/

import (
	//"golang.org/cmd/go/internal/module"
	"fmt"
	"io"

	mStream "../stream"
)

//BulletParams -- params for all bullets of andere weapons
type BulletParams struct {
	Speed  float64
	Size   float64
	Damage int32
}

//NewBulletParams -- return link to new BulletParams
func NewBulletParams(pSpeed float64, pSize float64, pDamage int32) *BulletParams {
	if pSize < 0 {
		panic(fmt.Errorf("NewBulletParams(): FATAL ERROR pSize(%v)<0", pSize))
	}
	if pDamage < 0 {
		panic(fmt.Errorf("NewBulletParams(): FATAL ERROR pDamage(%v)<0", pDamage))
	}
	return &BulletParams{
		Speed:  pSpeed,
		Size:   pSize,
		Damage: pDamage,
	}
}

//ReadBulletParams -- read BulletParams from net connection from LocalRunner
func ReadBulletParams(reader io.Reader) *BulletParams {
	size := mStream.ReadFloat64(reader)
	if size < 0 {
		panic(fmt.Errorf("ReadBulletParams(): FATAL ERROR size(%v)<0", size))
	}
	damage := mStream.ReadInt32(reader)
	if damage < 0 {
		panic(fmt.Errorf("ReadBulletParams(): FATAL ERROR damage(%v)<0", size))
	}
	return &BulletParams{
		Speed:  mStream.ReadFloat64(reader),
		Size:   size,
		Damage: damage,
	}
}

//Write -- write BulletParams to net connection to LocalRunner
func (value *BulletParams) Write(writer io.Writer) {
	mStream.WriteFloat64(writer, value.Speed)
	mStream.WriteFloat64(writer, value.Size)
	mStream.WriteInt32(writer, value.Damage)
}
