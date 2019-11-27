package model

import "io"

type LootBox struct {
    Position Vec2Float64
    Size Vec2Float64
    Item Item
}
func NewLootBox(position Vec2Float64, size Vec2Float64, item Item) LootBox {
    return LootBox {
        Position: position,
        Size: size,
        Item: item,
    }
}
func ReadLootBox(reader io.Reader) LootBox {
    result := LootBox {}
    result.Position = ReadVec2Float64(reader)
    result.Size = ReadVec2Float64(reader)
    result.Item = ReadItem(reader)
    return result
}
func (value LootBox) Write(writer io.Writer) {
    value.Position.Write(writer)
    value.Size.Write(writer)
    value.Item.Write(writer)
}
