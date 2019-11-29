namespace AiCup2019.Model

type PlayerView = {MyId: int; Game: Game} with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write this.MyId
        this.Game.writeTo writer

    static member readFrom (reader: System.IO.BinaryReader) =
        {
            MyId = reader.ReadInt32()
            Game = Game.readFrom reader
        }
