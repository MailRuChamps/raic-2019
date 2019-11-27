package main

import . "aicup2019/model"
import "fmt"

type MyStrategy struct {}

func NewMyStrategy() MyStrategy {
	return MyStrategy {}
}

func distanceSqr(a Vec2Float64, b Vec2Float64) float64 {
	return (a.X - b.X) * (a.X - b.X) + (a.Y - b.X) * (a.Y - b.Y)
}

func (strategy MyStrategy) getAction(unit Unit, game Game, debug Debug) UnitAction {
	var nearestEnemy *Unit
	for _, other := range game.Units {
		if other.PlayerId != unit.PlayerId {
			if nearestEnemy == nil || distanceSqr(unit.Position, other.Position) < distanceSqr(unit.Position, nearestEnemy.Position) {
				nearestEnemy = &other
			}
		}
	}
	var nearestWeapon *LootBox
	for _, lootBox := range game.LootBoxes {
		switch lootBox.Item.(type) {
		case *ItemWeapon:
			if nearestWeapon == nil || distanceSqr(unit.Position, lootBox.Position) < distanceSqr(unit.Position, nearestWeapon.Position) {
				nearestWeapon = &lootBox
			}
		}
	}
	targetPos := unit.Position;
	if unit.Weapon == nil && nearestWeapon != nil {
		targetPos = nearestWeapon.Position
	} else if nearestEnemy != nil {
		targetPos = nearestEnemy.Position
	}
	debug.Draw(CustomDataLog {
		Text: fmt.Sprintf("Target pos: %v", targetPos),
	});
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
		jump = true;
	}
	if targetPos.X < unit.Position.X && game.Level.Tiles[int(unit.Position.X - 1)][int(unit.Position.Y)] == TileWall {
		jump = true;
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