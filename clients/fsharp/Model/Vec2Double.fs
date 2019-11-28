namespace AiCup2019.Model

module Vec2Double = 
    type T = {X : double; Y : double} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write this.X
            writer.Write this.Y 

    let readFrom (reader: System.IO.BinaryReader) =
        {X = reader.ReadDouble(); Y = reader.ReadDouble()}