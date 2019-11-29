package model

/*
	DO NOT CHANGE this module.
	Its automatic replaced on games server
*/

import (
	"fmt"
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
	if currentTick < 0 {
		panic(fmt.Errorf("NewGame(): FATAL ERROR currentTick(%v)<0", currentTick))
	}
	if properties == nil {
		panic(fmt.Errorf("NewGame(): FATAL ERROR properties==nil"))
	}
	if level == nil {
		panic(fmt.Errorf("NewGame(): FATAL ERROR level==nil"))
	}
	if players == nil {
		panic(fmt.Errorf("NewGame(): FATAL ERROR players==nil"))
	}
	if units == nil {
		panic(fmt.Errorf("NewGame(): FATAL ERROR units==nil"))
	}
	if bullets == nil {
		panic(fmt.Errorf("NewGame(): FATAL ERROR bullets==nil"))
	}
	if mines == nil {
		panic(fmt.Errorf("NewGame(): FATAL ERROR mines==nil"))
	}
	if lootBoxes == nil {
		panic(fmt.Errorf("NewGame(): FATAL ERROR lootBoxes==nil"))
	}
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
	curTick := mStream.ReadInt32(reader)
	if curTick < 0 {
		panic(fmt.Errorf("ReadGame(): FATAL ERROR currentTick(%v)<0", curTick))
	}
	prop := ReadProperties(reader)
	if prop == nil {
		panic(fmt.Errorf("ReadGame(): FATAL ERROR prop==nil"))
	}
	level := ReadLevel(reader)
	if level == nil {
		panic(fmt.Errorf("ReadGame(): FATAL ERROR level==nil"))
	}
	result := &Game{
		CurrentTick: curTick,
		Properties:  prop,
		Level:       level,
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
