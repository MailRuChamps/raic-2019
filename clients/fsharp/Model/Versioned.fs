#nowarn "0058"
namespace AiCup2019.Model
type Versioned = {
    Inner: Map<int, UnitAction>;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 43981
        writer.Write this.Inner.Count
        this.Inner |> Map.iter (fun key value ->
            writer.Write key
            value.writeTo writer
        )
    static member readFrom(reader: System.IO.BinaryReader) = {
        Inner = [for _ in 1 .. reader.ReadInt32() do
            let key = reader.ReadInt32()
            let value = UnitAction.readFrom reader
            yield (key, value)
            ] |> Map.ofList
    }
