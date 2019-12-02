#nowarn "0058"
namespace AiCup2019.Model
type Vec2Single = {
    X: single;
    Y: single;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.X
        writer.Write this.Y
    static member readFrom(reader: System.IO.BinaryReader) = {
        X = reader.ReadSingle()
        Y = reader.ReadSingle()
    }
