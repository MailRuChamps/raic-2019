package model

/*
	Testing for bullets
*/

import (
	mTest "testing"
)

var (
	vector  *Vec2Float64
	speed   *Vec2Float64
	damage  int32   = 10
	size    float64 = 10
	explose *ExplosionParams
)

func TestBullet(test *mTest.T) {
	_Positive := func() {
		_NewBullet := func() {
			if bul := NewBullet(WeaponTypePistol, 10, 11, vector, speed, damage, size, explose); bul == nil {
				test.Errorf("_NewBullet(): ERROR bul == nil")
			}
		}
		_ReadBullet := func() {
			bul := ReadBullet(reader)
			if bul.WeaponType != 0 {
				test.Errorf("_NewBullet(): ERROR bul.WeaponType(0)=%v", bul.WeaponType)
			}
			if bul.UnitID != 0 {
				test.Errorf("_NewBullet(): ERROR bul.UnitID(0)=%v", bul.UnitID)
			}
			if bul.PlayerID != 0 {
				test.Errorf("_NewBullet(): ERROR bul.PlayerID(0)=%v", bul.PlayerID)
			}
			if bul.Position == nil {
				test.Errorf("_NewBullet(): ERROR bul.Position==nil")
			}
			if bul.Velocity == nil {
				test.Errorf("_NewBullet(): ERROR bul.Velocity==nil")
			}
			if bul.Damage != 0 {
				test.Errorf("_NewBullet(): ERROR bul.Damage(0)=%v", bul.Damage)
			}
			if bul.Size != 0 {
				test.Errorf("_NewBullet(): ERROR bul.Size(0)=%v", bul.Size)
			}
		}
		_WriteBullet := func() {
			bul := ReadBullet(reader)
			bul.Write(writer)
		}
		_NewBullet()
		_ReadBullet()
		_WriteBullet()
	}
	_Negative := func() {
		_BadWeapon := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadWeapon(): ERROR in panic")
				}
			}()
			_ = NewBullet(-1, 10, 11, vector, speed, damage, size, explose)
		}
		_BadWeapon2 := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadWeapon2(): ERROR in panic")
				}
			}()
			_ = NewBullet(3, 10, 11, vector, speed, damage, size, explose)
		}
		_BadPosition := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadPosition(): ERROR in panic")
				}
			}()
			_ = NewBullet(WeaponTypePistol, 10, 11, nil, speed, damage, size, explose)
		}
		_BadSpeed := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadSpeed(): ERROR in panic")
				}
			}()
			_ = NewBullet(WeaponTypePistol, 10, 11, vector, nil, damage, size, explose)
		}
		_BadDamage := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadDamage(): ERROR in panic")
				}
			}()
			_ = NewBullet(WeaponTypePistol, 10, 11, vector, speed, -1, size, explose)
		}
		_BadSize := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadSize(): ERROR in panic")
				}
			}()
			_ = NewBullet(WeaponTypePistol, 10, 11, vector, speed, damage, -1, explose)
		}
		_BadExplosion := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadExplosion(): ERROR in panic")
				}
			}()
			_ = NewBullet(WeaponTypePistol, 10, 11, vector, speed, damage, size, nil)
		}
		_BadWeapon()
		_BadWeapon2()
		_BadPosition()
		_BadSpeed()
		_BadDamage()
		_BadSize()
		_BadExplosion()
	}
	vector = NewVec2Float64(1, 1)
	speed = NewVec2Float64(1, 1)
	explose = NewExplosionParams(1, 1)
	_Positive()
	_Negative()
}
