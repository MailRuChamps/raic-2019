package model

/*
	This module testing of bullets params
*/

import (
	mTest "testing"
)

type iReader interface {
	Read()
}
type iReader2 interface {
	Read()
}
type iWriter interface{}

type tReader struct{}
type tReader2 struct{}
type tWriter struct{}

var (
	reader  *tReader
	reader2 *tReader2
	writer  *tWriter
)

func (tr *tReader) Read(pBytes []byte) (int, error) {
	pBytes = []byte{1, 2, 3}
	return 3, nil
}

func (tr *tReader2) Read(pBytes []byte) (int, error) {
	pBytes = []byte{1, 2, 3}
	return -1, nil
}

func (tw *tWriter) Write([]byte) (int, error) {
	return 3, nil
}

func TestBulletParams(test *mTest.T) {
	_Posiiv := func() {
		_NewBulParam := func() {
			bp := NewBulletParams(1, 2, 3)
			if bp == nil {
				test.Errorf("_NewBulParam(): ERROR bp have nil")
			}
			if bp.Speed != 1 {
				test.Errorf("_NewBulParam(): ERROR in speed(1)=%v", bp.Speed)
			}
			if bp.Size != 2 {
				test.Errorf("_NewBulParam(): ERROR in size(2)=%v", bp.Size)
			}
			if bp.Damage != 3 {
				test.Errorf("_NewBulParam(): ERROR in damage(3)=%v", bp.Damage)
			}

		}
		_ReadNet := func() {
			bp := ReadBulletParams(reader)
			if bp.Speed != 0 {
				test.Errorf("_ReadNet(): ERROR in speed(0)=%v", bp.Speed)
			}
			if bp.Size != 0 {
				test.Errorf("_NewBulParam(): ERROR in size(0)=%v", bp.Size)
			}
			if bp.Damage != 0 {
				test.Errorf("_NewBulParam(): ERROR in damage(0)=%v", bp.Damage)
			}
		}
		_WriteNet := func() {
			bp := NewBulletParams(1, 2, 3)
			bp.Write(writer)
		}
		_NewBulParam()
		_ReadNet()
		_WriteNet()

	}
	_Negative := func() {
		_NegativeSize := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_NegativeSize(): ERROR in panic")
				}
			}()
			NewBulletParams(0, -1, 2)
		}
		_NegativeDamage := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_NegativeDamage(): ERROR in panic")
				}
			}()
			NewBulletParams(1, 1, -1)
		}
		_NegativeSize2 := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_NegativeSize2(): ERROR in panic")
				}
			}()
			bp := ReadBulletParams(reader2)
			if bp.Speed != 0 {
				test.Errorf("_NegativeSize2(): ERROR in speed(0)=%v", bp.Speed)
			}
			if bp.Size != 0 {
				test.Errorf("_NegativeSize2(): ERROR in size(0)=%v", bp.Size)
			}
			if bp.Damage != 0 {
				test.Errorf("_NegativeSize2(): ERROR in damage(0)=%v", bp.Damage)
			}
		}
		_NegativeSize()
		_NegativeDamage()
		_NegativeSize2()
	}
	_Posiiv()
	_Negative()
}

func init() {
	writer = &tWriter{}
	reader = &tReader{}
	reader2 = &tReader2{}
}
