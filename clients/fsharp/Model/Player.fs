namespace AiCup2019.Model

module Player =
    type T = {Id: int; Score: int} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write this.Id
            writer.Write this.Score

    let readFrom (reader: System.IO.BinaryReader) =
        {
            Id = reader.ReadInt32()
            Score = reader.ReadInt32()
        }
        

