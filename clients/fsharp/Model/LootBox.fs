namespace AiCup2019.Model

module LootBox =
    type T = {Position: Vec2Double.T; Size: Vec2Double.T; Item: Item.T} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            this.Position.writeTo writer
            this.Size.writeTo writer
            this.Item.writeTo writer

    let readFrom (reader: System.IO.BinaryReader) =
        {
            Position = Vec2Double.readFrom reader
            Size = Vec2Double.readFrom reader
            Item = Item.T.readFrom reader
        }


