package model

import "io"
import . "aicup2019/stream"

type Game struct {
    CurrentTick int32
    Properties Properties
    Level Level
    Players []Player
    Units []Unit
    Bullets []Bullet
    Mines []Mine
    LootBoxes []LootBox
}
func NewGame(currentTick int32, properties Properties, level Level, players []Player, units []Unit, bullets []Bullet, mines []Mine, lootBoxes []LootBox) Game {
    return Game {
        CurrentTick: currentTick,
        Properties: properties,
        Level: level,
        Players: players,
        Units: units,
        Bullets: bullets,
        Mines: mines,
        LootBoxes: lootBoxes,
    }
}
func ReadGame(reader io.Reader) Game {
    result := Game {}
    result.CurrentTick = ReadInt32(reader)
    result.Properties = ReadProperties(reader)
    result.Level = ReadLevel(reader)
    result.Players = make([]Player, ReadInt32(reader))
    for i := range result.Players {
        result.Players[i] = ReadPlayer(reader)
    }
    result.Units = make([]Unit, ReadInt32(reader))
    for i := range result.Units {
        result.Units[i] = ReadUnit(reader)
    }
    result.Bullets = make([]Bullet, ReadInt32(reader))
    for i := range result.Bullets {
        result.Bullets[i] = ReadBullet(reader)
    }
    result.Mines = make([]Mine, ReadInt32(reader))
    for i := range result.Mines {
        result.Mines[i] = ReadMine(reader)
    }
    result.LootBoxes = make([]LootBox, ReadInt32(reader))
    for i := range result.LootBoxes {
        result.LootBoxes[i] = ReadLootBox(reader)
    }
    return result
}
func (value Game) Write(writer io.Writer) {
    WriteInt32(writer, value.CurrentTick)
    value.Properties.Write(writer)
    value.Level.Write(writer)
    WriteInt32(writer, int32(len(value.Players)))
    for _, PlayersElement := range value.Players {
        PlayersElement.Write(writer)
    }
    WriteInt32(writer, int32(len(value.Units)))
    for _, UnitsElement := range value.Units {
        UnitsElement.Write(writer)
    }
    WriteInt32(writer, int32(len(value.Bullets)))
    for _, BulletsElement := range value.Bullets {
        BulletsElement.Write(writer)
    }
    WriteInt32(writer, int32(len(value.Mines)))
    for _, MinesElement := range value.Mines {
        MinesElement.Write(writer)
    }
    WriteInt32(writer, int32(len(value.LootBoxes)))
    for _, LootBoxesElement := range value.LootBoxes {
        LootBoxesElement.Write(writer)
    }
}
