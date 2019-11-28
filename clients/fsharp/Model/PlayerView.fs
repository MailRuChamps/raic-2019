namespace AiCup2019.Model

module PlayerView =
    type T = {MyId: int; Game: Game.T} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write this.MyId
            this.Game.writeTo writer

    let readFrom (reader: System.IO.BinaryReader) =
    {
        MyId = reader.ReadInt32()
        Game = Game.readFrom reader
    }


