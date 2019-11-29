package model

import "io"

type LootBox struct {
	Position *Vec2Float64
	Size     *Vec2Float64
	Item     IItem
}

func NewLootBox(position *Vec2Float64, size *Vec2Float64, item IItem) *LootBox {
	return &LootBox{
		Position: position,
		Size:     size,
		Item:     item,
	}
}
func ReadLootBox(reader io.Reader) *LootBox {
	return &LootBox{
		Position: ReadVec2Float64(reader),
		Size:     ReadVec2Float64(reader),
		Item:     ReadItem(reader),
	}
}
func (value *LootBox) Write(writer io.Writer) {
	value.Position.Write(writer)
	value.Size.Write(writer)
	value.Item.Write(writer)
}
