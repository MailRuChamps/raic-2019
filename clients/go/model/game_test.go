package model

/*
	Test for Game
*/

import (
	mTest "testing"
)

var (
	curTick    int32
	properties *Properties
	level      *Level
	players    []*Player
	units      []*Unit
	bullets    []*Bullet
	mines      []*Mine
	lootBoxes  []*LootBox
	size2      *Vec2Float64
	weapParams map[WeaponType]*WeaponParams
	expParams  *ExplosionParams
)

func TestGame(test *mTest.T) {
	_Positive := func() {
		_NewGame := func() {
			game := NewGame(curTick, properties, level, players, units, bullets, mines, lootBoxes)
			if game == nil {
				test.Errorf("_NewGame(): ERROR game==nil")
			}
			if game.CurrentTick < 0 {
				test.Errorf("_NewGame(): ERROR game.CurrentTick(%v)<0", game.CurrentTick)
			}
			if game.Properties == nil {
				test.Errorf("_NewGame(): ERROR game.Properties==nil")
			}
			if game.Level == nil {
				test.Errorf("_NewGame(): ERROR game.Level==nil")
			}
			if game.Players == nil {
				test.Errorf("_NewGame(): ERROR game.Players==nil")
			}
			if game.Units == nil {
				test.Errorf("_NewGame(): ERROR game.Units==nil")
			}
			if game.Bullets == nil {
				test.Errorf("_NewGame(): ERROR game.Bullets==nil")
			}
			if game.Mines == nil {
				test.Errorf("_NewGame(): ERROR game.Mines==nil")
			}
			if game.LootBoxes == nil {
				test.Errorf("_NewGame(): ERROR game.LootBoxes==nil")
			}
		}
		_ReadGame := func() {
			game := ReadGame(reader)
			if game == nil {
				test.Errorf("_NewGame(): ERROR game==nil")
			}
			if game.CurrentTick < 0 {
				test.Errorf("_NewGame(): ERROR game.CurrentTick(%v)<0", game.CurrentTick)
			}
			if game.Properties == nil {
				test.Errorf("_NewGame(): ERROR game.Properties==nil")
			}
			if game.Level == nil {
				test.Errorf("_NewGame(): ERROR game.Level==nil")
			}
			if game.Players == nil {
				test.Errorf("_NewGame(): ERROR game.Players==nil")
			}
			if game.Units == nil {
				test.Errorf("_NewGame(): ERROR game.Units==nil")
			}
			if game.Bullets == nil {
				test.Errorf("_NewGame(): ERROR game.Bullets==nil")
			}
			if game.Mines == nil {
				test.Errorf("_NewGame(): ERROR game.Mines==nil")
			}
			if game.LootBoxes == nil {
				test.Errorf("_NewGame(): ERROR game.LootBoxes==nil")
			}
		}
		_WriteGame := func() {
			game := ReadGame(reader)
			game.Write(writer)
		}
		_NewGame()
		_ReadGame()
		_WriteGame()
	}
	_Negative := func() {
		_BadTick := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadTick(): ERROR in panic")
				}
			}()
			_ = NewGame(-1, properties, level, players, units, bullets, mines, lootBoxes)
		}
		_BadProp := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadProp(): ERROR in panic")
				}
			}()
			_ = NewGame(curTick, nil, level, players, units, bullets, mines, lootBoxes)
		}
		_BadLevel := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadLevel(): ERROR in panic")
				}
			}()
			_ = NewGame(curTick, properties, nil, players, units, bullets, mines, lootBoxes)
		}
		_BadPlayer := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadLevel(): ERROR in panic")
				}
			}()
			_ = NewGame(curTick, properties, level, nil, units, bullets, mines, lootBoxes)
		}
		_BadUnits := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadUnits(): ERROR in panic")
				}
			}()
			_ = NewGame(curTick, properties, level, players, nil, bullets, mines, lootBoxes)
		}
		_BadBul := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadBul(): ERROR in panic")
				}
			}()
			_ = NewGame(curTick, properties, level, players, units, nil, mines, lootBoxes)
		}
		_BadMine := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadMine(): ERROR in panic")
				}
			}()
			_ = NewGame(curTick, properties, level, players, units, bullets, nil, lootBoxes)
		}
		_BadLoot := func() {
			defer func() {
				if pan := recover(); pan == nil {
					test.Errorf("_BadLoot(): ERROR in panic")
				}
			}()
			_ = NewGame(curTick, properties, level, players, units, bullets, mines, nil)
		}
		_BadTick()
		_BadProp()
		_BadLevel()
		_BadPlayer()
		_BadUnits()
		_BadBul()
		_BadMine()
		_BadLoot()
	}
	_Positive()
	_Negative()
}

func init() {
	curTick = 100
	size2 = NewVec2Float64(1, 1)
	weapParams = make(map[WeaponType]*WeaponParams, 4)
	expParams = NewExplosionParams(10, 10)
	tiles := make([][]Tile, 5)
	level = NewLevel(tiles)
	players = make([]*Player, 5)
	units = make([]*Unit, 5)
	bullets = make([]*Bullet, 5)
	mines = make([]*Mine, 5)
	lootBoxes = make([]*LootBox, 5)
	properties = NewProperties(1000, 10, 5, 5, size2, size2, 10, 10, 1, 10, 1, 1, 100, 30, weapParams, size2, expParams, 10, 10, 10, 10)
}
