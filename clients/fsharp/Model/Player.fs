#nowarn "0058"
namespace AiCup2019.Model
type Player = {
    Id: int;
    Score: int;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.Id
        writer.Write this.Score
    static member readFrom(reader: System.IO.BinaryReader) = {
        Id = reader.ReadInt32()
        Score = reader.ReadInt32()
    }
