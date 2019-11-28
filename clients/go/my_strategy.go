package main

import . "aicup2019/model"
import "fmt"

type MyStrategy struct {}

func NewMyStrategy() MyStrategy {
	return MyStrategy {}
}

func distanceSqr(a Vec2Float64, b Vec2Float64) float64 {
	return (a.X - b.X) * (a.X - b.X) + (a.Y - b.Y) * (a.Y - b.Y)
}

func (strategy MyStrategy) getAction(unit Unit, game Game, debug Debug) UnitAction {
	var nearestEnemy *Unit
	for i := 0; i < len(game.Units); i++ {
		if game.Units[i].PlayerId != unit.PlayerId {
			if nearestEnemy == nil || distanceSqr(unit.Position, game.Units[i].Position) < distanceSqr(unit.Position, nearestEnemy.Position) {
				nearestEnemy = &game.Units[i]
			}
		}
	}
	var nearestWeapon *LootBox
	for i := 0; i < len(game.LootBoxes); i++  {
		switch game.LootBoxes[i].Item.(type) {
		case ItemWeapon:
			if nearestWeapon == nil || distanceSqr(unit.Position, game.LootBoxes[i].Position) < distanceSqr(unit.Position, nearestWeapon.Position) {
				nearestWeapon = &game.LootBoxes[i]
			}
		}
	}
	targetPos := unit.Position
	if unit.Weapon == nil && nearestWeapon != nil {
		targetPos = nearestWeapon.Position
	} else if nearestEnemy != nil {
		targetPos = nearestEnemy.Position
	}
	debug.Draw(CustomDataLog {
		Text: fmt.Sprintf("Target pos: %v", targetPos),
	})
	aim := Vec2Float64 {
		X: 0,
		Y: 0,
	}
	if nearestEnemy != nil {
		aim = Vec2Float64 {
			X: nearestEnemy.Position.X - unit.Position.X,
			Y: nearestEnemy.Position.Y - unit.Position.Y,
		}
	}
	jump := targetPos.Y > unit.Position.Y
	if targetPos.X > unit.Position.X && game.Level.Tiles[int(unit.Position.X + 1)][int(unit.Position.Y)] == TileWall {
		jump = true
	}
	if targetPos.X < unit.Position.X && game.Level.Tiles[int(unit.Position.X - 1)][int(unit.Position.Y)] == TileWall {
		jump = true
	}
	return UnitAction {
		Velocity: targetPos.X - unit.Position.X,
		Jump: jump,
		JumpDown: !jump,
		Aim: aim,
		SwapWeapon: false,
		PlantMine: false,
	}
}