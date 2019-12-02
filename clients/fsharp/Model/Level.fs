#nowarn "0058"
namespace AiCup2019.Model
type Level = {
    Tiles: Tile[][];
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.Tiles.Length
        this.Tiles |> Array.iter (fun value ->
            writer.Write value.Length
            value |> Array.iter (fun value ->
                writer.Write (int value)
            )
        )
    static member readFrom(reader: System.IO.BinaryReader) = {
        Tiles = [|for _ in 1 .. reader.ReadInt32() do
            yield [|for _ in 1 .. reader.ReadInt32() do
                yield reader.ReadInt32() |> enum
            |]
        |]
    }
