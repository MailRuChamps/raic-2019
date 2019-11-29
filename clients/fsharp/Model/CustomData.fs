namespace AiCup2019.Model

type CustomDataType = Log = 0 | Rect = 1 | Line = 2 | Polygon = 3

module CustomData = 
    type Log = {Text: string} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write(int CustomDataType.Log)
            let textData : byte[]= System.Text.Encoding.UTF8.GetBytes this.Text
            writer.Write textData.Length
            writer.Write textData

        static member readFrom (reader: System.IO.BinaryReader) =
                { Text = reader.ReadInt32() |> reader.ReadBytes |>  System.Text.Encoding.UTF8.GetString }

    type Rect = {Pos: Vec2Single; Size: Vec2Single; Color: ColorFloat} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write(int CustomDataType.Rect)
            this.Pos.writeTo writer
            this.Size.writeTo writer
            this.Color.writeTo writer
        
        static member readFrom (reader: System.IO.BinaryReader) =
           {
                Pos = Vec2Single.readFrom reader
                Size = Vec2Single.readFrom reader
                Color = ColorFloat.readFrom reader
           }

    type Line = {P1: Vec2Single; P2: Vec2Single; Width: single; Color: ColorFloat} with
        member this.writeTo (writer: System.IO.BinaryWriter) = 
            writer.Write(int CustomDataType.Line)
            this.P1.writeTo writer
            this.P2.writeTo writer
            writer.Write this.Width
            this.Color.writeTo writer

        static member readFrom (reader: System.IO.BinaryReader) = 
            {
                P1 = Vec2Single.readFrom reader
                P2 = Vec2Single.readFrom reader
                Width = reader.ReadSingle()
                Color = ColorFloat.readFrom reader
            }

    type Polygon = {Vertices: ColoredVertex[]} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write(int CustomDataType.Polygon)
            writer.Write(this.Vertices.Length)
            this.Vertices |> Array.iter (fun i -> i.writeTo writer)

        static member readFrom (reader: System.IO.BinaryReader) = 
           { Vertices = [|for _ = 1 to reader.ReadInt32() do yield ColoredVertex.readFrom reader|] }
    
    type T = Log of Log | Rect of Rect | Line of Line | Polygon of Polygon with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            match this with
                | Log x -> x.writeTo writer
                | Rect x -> x.writeTo writer
                | Line x -> x.writeTo writer
                | Polygon x -> x.writeTo writer

        static member readFrom (reader: System.IO.BinaryReader) =
            match reader.ReadInt32() |> enum with
                | CustomDataType.Log -> Log (Log.readFrom reader)
                | CustomDataType.Rect -> Rect (Rect.readFrom reader)
                | CustomDataType.Line -> Line (Line.readFrom reader)
                | CustomDataType.Polygon -> Polygon (Polygon.readFrom reader)
                | x -> failwith (sprintf "Unexpected CustomDataType %d" (int x))
