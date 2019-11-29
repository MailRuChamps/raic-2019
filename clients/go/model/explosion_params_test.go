package model

/*
	Testing explosion_params
*/

import (
	mTest "testing"
)

func TestExpl(test *mTest.T) {
	_Positive := func() {
		_NewExp := func() {
			exp := NewExplosionParams(10, 10)
			if exp == nil {
				test.Errorf("_NewExp(): ERROR exp==nil")
			}
			if exp.Radius != 10 {
				test.Errorf("_NewExp(): ERROR exp.Radius(%v)!=10", exp.Radius)
			}
			if exp.Damage != 10 {
				test.Errorf("_NewExp(): ERROR exp.Damage(%v)!=10", exp.Damage)
			}
		}
		_ReadExp := func() {
			exp := ReadExplosionParams(reader)
			if exp == nil {
				test.Errorf("_ReadExp(): ERROR exp==nil")
			}
			if exp.Radius != 0 {
				test.Errorf("_NewExp(): ERROR exp.Radius(%v)!=0", exp.Radius)
			}
			if exp.Damage != 0 {
				test.Errorf("_NewExp(): ERROR exp.Damage(%v)!=0", exp.Damage)
			}
		}
		_WriteExp := func() {
			exp := NewExplosionParams(10, 10)
			exp.Write(writer)
		}
		_NewExp()
		_ReadExp()
		_WriteExp()
	}
	_Negative := func() {
		_BadRadius := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadRadius(): ERROR in panic")
				}
			}()
			_ = NewExplosionParams(-1, 10)
		}
		_BadDamage := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadDamage(): ERROR in panic")
				}
			}()
			_ = NewExplosionParams(10, -1)
		}
		_BadRadius()
		_BadDamage()
	}
	_Positive()
	_Negative()
}
