namespace AiCup2019.Model

module ColoredVertex =
    type T = {Position: Vec2Single.T; Color: ColorFloat.T} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            this.Position.writeTo writer
            this.Color.writeTo writer

    let readFrom (reader: System.IO.BinaryReader) = 
        {
            Position = Vec2Single.readFrom reader
            Color = ColorFloat.readFrom reader
        }

