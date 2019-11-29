namespace AiCup2019.Model

module Level =
    type T = {Tiles: Tile[][]} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write this.Tiles.Length
            this.Tiles |> Array.iter(fun row -> 
                                        writer.Write row.Length
                                        row |> Array.iter(fun el -> writer.Write (int el)))

    let readFrom (reader: System.IO.BinaryReader) =
        {
            Tiles = ([|for i = 1 to reader.ReadInt32() do
                        yield [|for j = 1 to reader.ReadInt32() do
                                yield match reader.ReadInt32() with
                                        | 0 -> Tile.Empty
                                        | 1 -> Tile.Wall
                                        | 2 -> Tile.Platform
                                        | 3 -> Tile.Ladder
                                        | 4 -> Tile.JumpPad
                                        | x -> failwith (sprintf "Unexpected TileType %d" x)
                                   |]
                   |])
        }
