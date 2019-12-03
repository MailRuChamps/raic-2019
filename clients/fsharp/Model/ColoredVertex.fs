#nowarn "0058"
namespace AiCup2019.Model
type ColoredVertex = {
    Position: Vec2Single;
    Color: ColorSingle;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        this.Position.writeTo writer
        this.Color.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) = {
        Position = Vec2Single.readFrom reader
        Color = ColorSingle.readFrom reader
    }
