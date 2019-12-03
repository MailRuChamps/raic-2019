#nowarn "0058"
namespace AiCup2019.Model
type Vec2Double = {
    X: double;
    Y: double;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.X
        writer.Write this.Y
    static member readFrom(reader: System.IO.BinaryReader) = {
        X = reader.ReadDouble()
        Y = reader.ReadDouble()
    }
