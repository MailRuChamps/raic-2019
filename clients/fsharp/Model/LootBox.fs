#nowarn "0058"
namespace AiCup2019.Model
type LootBox = {
    Position: Vec2Double;
    Size: Vec2Double;
    Item: Item;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        this.Position.writeTo writer
        this.Size.writeTo writer
        this.Item.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) = {
        Position = Vec2Double.readFrom reader
        Size = Vec2Double.readFrom reader
        Item = Item.readFrom reader
    }
