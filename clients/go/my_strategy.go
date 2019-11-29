package main

import (
	mModel "./model"
	"fmt"
)

//MyStrategy -- object to send in LocalRunner
type MyStrategy struct{}

//NewMyStrategy -- return link to MyStrategy
func NewMyStrategy() *MyStrategy {
	return &MyStrategy{}
}

//Calculate distance betwin of two games objects
func distanceSqr(a, b *mModel.Vec2Float64) float64 {
	return (a.X-b.X)*(a.X-b.X) + (a.Y-b.Y)*(a.Y-b.Y)
}

//Return action of MyStrategy for LocalRunner on now tik
func (strategy *MyStrategy) getAction(unit *mModel.Unit, game *mModel.Game, debug Debug) *mModel.UnitAction {
	var nearestEnemy *mModel.Unit
	for _, other := range game.Units {
		if other.PlayerId != unit.PlayerId {
			if nearestEnemy == nil || distanceSqr(unit.Position, other.Position) < distanceSqr(unit.Position, nearestEnemy.Position) {
				nearestEnemy = other
			}
		}
	}
	var nearestWeapon *mModel.LootBox
	for _, lootBox := range game.LootBoxes {
		switch lootBox.Item.(type) {
		case *mModel.ItemWeapon:
			if nearestWeapon == nil || distanceSqr(unit.Position, lootBox.Position) < distanceSqr(unit.Position, nearestWeapon.Position) {
				nearestWeapon = lootBox
			}
		}
	}
	targetPos := unit.Position
	if unit.Weapon == nil && nearestWeapon != nil {
		targetPos = nearestWeapon.Position
	} else if nearestEnemy != nil {
		targetPos = nearestEnemy.Position
	}
	debug.Draw(&mModel.CustomDataLog{
		Text: fmt.Sprintf("Target pos: %v", targetPos),
	})
	aim := &mModel.Vec2Float64{
		X: 0,
		Y: 0,
	}
	if nearestEnemy != nil {
		aim = &mModel.Vec2Float64{
			X: nearestEnemy.Position.X - unit.Position.X,
			Y: nearestEnemy.Position.Y - unit.Position.Y,
		}
	}
	jump := targetPos.Y > unit.Position.Y
	if targetPos.X > unit.Position.X && game.Level.Tiles[int(unit.Position.X+1)][int(unit.Position.Y)] == mModel.TileWall {
		jump = true
	}
	if targetPos.X < unit.Position.X && game.Level.Tiles[int(unit.Position.X-1)][int(unit.Position.Y)] == mModel.TileWall {
		jump = true
	}
	return &mModel.UnitAction{
		Velocity:   targetPos.X - unit.Position.X,
		Jump:       jump,
		JumpDown:   !jump,
		Aim:        aim,
		SwapWeapon: false,
		PlantMine:  false,
	}
}
