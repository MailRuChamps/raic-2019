package model

import (
	"io"

	mStream "../stream"
)

//Game -- global objects for game
type Game struct {
	CurrentTick int32
	Properties  *Properties
	Level       *Level
	Players     []*Player
	Units       []*Unit
	Bullets     []*Bullet
	Mines       []*Mine
	LootBoxes   []*LootBox
}

//NewGame -- return link to new Game
func NewGame(currentTick int32, properties *Properties, level *Level, players []*Player,
	units []*Unit, bullets []*Bullet, mines []*Mine, lootBoxes []*LootBox) *Game {
	return &Game{
		CurrentTick: currentTick,
		Properties:  properties,
		Level:       level,
		Players:     players,
		Units:       units,
		Bullets:     bullets,
		Mines:       mines,
		LootBoxes:   lootBoxes,
	}
}

//ReadGame -- read from net Game from LocalRunner
func ReadGame(reader io.Reader) *Game {
	result := &Game{
		CurrentTick: mStream.ReadInt32(reader),
		Properties:  ReadProperties(reader),
		Level:       ReadLevel(reader),
		Players:     make([]*Player, mStream.ReadInt32(reader)),
	}
	for i := range result.Players {
		result.Players[i] = ReadPlayer(reader)
	}
	result.Units = make([]*Unit, mStream.ReadInt32(reader))
	for i := range result.Units {
		result.Units[i] = ReadUnit(reader)
	}
	result.Bullets = make([]*Bullet, mStream.ReadInt32(reader))
	for i := range result.Bullets {
		result.Bullets[i] = ReadBullet(reader)
	}
	result.Mines = make([]*Mine, mStream.ReadInt32(reader))
	for i := range result.Mines {
		result.Mines[i] = ReadMine(reader)
	}
	result.LootBoxes = make([]*LootBox, mStream.ReadInt32(reader))
	for i := range result.LootBoxes {
		result.LootBoxes[i] = ReadLootBox(reader)
	}
	return result
}
func (value Game) Write(writer io.Writer) {
	mStream.WriteInt32(writer, value.CurrentTick)
	value.Properties.Write(writer)
	value.Level.Write(writer)
	mStream.WriteInt32(writer, int32(len(value.Players)))
	for _, PlayersElement := range value.Players {
		PlayersElement.Write(writer)
	}
	mStream.WriteInt32(writer, int32(len(value.Units)))
	for _, UnitsElement := range value.Units {
		UnitsElement.Write(writer)
	}
	mStream.WriteInt32(writer, int32(len(value.Bullets)))
	for _, BulletsElement := range value.Bullets {
		BulletsElement.Write(writer)
	}
	mStream.WriteInt32(writer, int32(len(value.Mines)))
	for _, MinesElement := range value.Mines {
		MinesElement.Write(writer)
	}
	mStream.WriteInt32(writer, int32(len(value.LootBoxes)))
	for _, LootBoxesElement := range value.LootBoxes {
		LootBoxesElement.Write(writer)
	}
}
