package model

/*
	Testing item
*/

import (
	mTest "testing"
)

func TestItemMine(test *mTest.T) {
	_Positive := func() {
		_NewMine := func() {
			iw := NewItemMine()
			if iw == nil {
				test.Errorf("_NewMine(): ERROR iw==nil")
			}
		}
		_ReadMine := func() {
			iw := ReadItemMine(reader)
			if iw == nil {
				test.Errorf("_ReadMine(): ERROR iw==nil")
			}
		}
		_WriteMine := func() {
			iw := NewItemMine()
			iw.Write(writer)
		}
		_NewMine()
		_ReadMine()
		_WriteMine()
	}
	_Positive()
}

func TestItemWeapon(test *mTest.T) {
	_Positive := func() {
		_NewIw := func() {
			iw := NewItemWeapon(WeaponTypePistol)
			if iw == nil {
				test.Errorf("_NewIw(): ERROR iw==nil")
			}
		}
		_ReadIw := func() {
			iw := ReadItemWeapon(reader)
			if iw == nil {
				test.Errorf("_ReadIw(): ERROR iw==nil")
			}
		}
		_Write := func() {
			iw := NewItemWeapon(WeaponTypePistol)
			iw.Write(writer)
		}
		_NewIw()
		_ReadIw()
		_Write()
	}
	_Negative := func() {
		_BadWeapon := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadWeapon(): ERROR in panic")
				}
			}()
			_ = NewItemWeapon(-1)
		}
		_BadWeapon()
	}
	_Positive()
	_Negative()
}

func TestHealtPack(test *mTest.T) {
	_Positive := func() {
		_NewHp := func() {
			hp := NewItemHealthPack(30)
			if hp == nil {
				test.Errorf("_NewHp(): ERROR hp==nil")
			}
			if hp.Health != 30 {
				test.Errorf("_NewHp(): ERROR hp.Health(%v)!=30", hp.Health)
			}
		}
		_ReadHp := func() {
			hp := ReadItemHealthPack(reader)
			if hp == nil {
				test.Errorf("_ReadHp(): ERROR hp==nil")
			}
			if hp.Health != 0 {
				test.Errorf("_ReadHp(): ERROR hp.Health(%v)!=0", hp.Health)
			}
		}
		_WriteHp := func() {
			hp := NewItemHealthPack(30)
			hp.Write(writer)
		}
		_NewHp()
		_ReadHp()
		_WriteHp()
	}
	_Negative := func() {
		_BadHealt := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadHealt(): ERROR in panic")
				}
			}()
			_ = NewItemHealthPack(-1)
		}
		_BadHealt()
	}
	_Positive()
	_Negative()
}

func TestItem(test *mTest.T) {
	_Positive := func() {
		_ReadItem := func() {
			item := ReadItem(reader)
			if item == nil {
				test.Errorf("_REadItem(): ERROR item==nil")
			}
		}
		_ReadItem()
	}
	_Positive()
}
