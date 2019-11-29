package model

/*
	Test for color_float32
*/

import (
	mTest "testing"
)

func TestColorFloat32(test *mTest.T) {
	_Positive := func() {
		_CreateColor := func() {
			col := NewColorFloat32(10, 11, 12, 1)
			if col == nil {
				test.Errorf("_CreateColor(): ERROR col==nil")
			}
			if col.A != 1 {
				test.Errorf("_ReadColor(): ERROR col.A(%v)!=1", col.A)
			}
			if col.B != 12 {
				test.Errorf("_ReadColor(): ERROR col.B(%v)!=12", col.B)
			}
			if col.G != 11 {
				test.Errorf("_ReadColor(): ERROR col.G(%v)!=11", col.G)
			}
			if col.R != 10 {
				test.Errorf("_ReadColor(): ERROR col.R(%v)!=10", col.R)
			}
		}
		_ReadColor := func() {
			col := ReadColorFloat32(reader)
			if col.A != 0 {
				test.Errorf("_ReadColor(): ERROR col.A(%v)!=0", col.A)
			}
			if col.B != 0 {
				test.Errorf("_ReadColor(): ERROR col.B(%v)!=0", col.B)
			}
			if col.G != 0 {
				test.Errorf("_ReadColor(): ERROR col.G(%v)!=0", col.G)
			}
			if col.R != 0 {
				test.Errorf("_ReadColor(): ERROR col.R(%v)!=0", col.R)
			}
		}
		_WriteCol := func() {
			col := ReadColorFloat32(reader)
			col.Write(writer)
		}
		_CreateColor()
		_ReadColor()
		_WriteCol()
	}
	_Negative := func() {
		_BadR := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadR(): ERROR in panic")
				}
			}()
			_ = NewColorFloat32(-1, 10, 10, 10)
		}
		_BadG := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadG(): ERROR in panic")
				}
			}()
			_ = NewColorFloat32(10, -1, 10, 10)
		}
		_BadB := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadB(): ERROR in panic")
				}
			}()
			_ = NewColorFloat32(10, 10, -1, 10)
		}
		_BadA := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadA(): ERROR in panic")
				}
			}()
			_ = NewColorFloat32(10, 10, 10, -1)
		}
		_BadR()
		_BadG()
		_BadB()
		_BadA()
	}
	_Positive()
	_Negative()
}
