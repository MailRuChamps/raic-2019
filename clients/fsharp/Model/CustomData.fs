namespace AiCup2019.Model

type CustomDataType = Log = 0 | Rect = 1 | Line = 2 | Polygon = 3

module CustomData =
    [<AbstractClass>]
    type T() =
        abstract member writeTo: System.IO.BinaryWriter -> unit
 
    type Log(text) =
        inherit T()
        member this.Text : string = text

        override this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write(int CustomDataType.Log)
            let textData : byte[]= System.Text.Encoding.UTF8.GetBytes this.Text
            writer.Write textData.Length
            writer.Write textData

        static member readFrom (reader: System.IO.BinaryReader) =
            new Log(reader.ReadInt32() |> reader.ReadBytes |>  System.Text.Encoding.UTF8.GetString)

    type Rect(pos, size, color) =
        inherit T()
        member this.Pos : Vec2Single.T = pos
        member this.Size : Vec2Single.T = size
        member this.Color : ColorFloat.T = color

        override this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write(int CustomDataType.Rect)
            this.Pos.writeTo writer
            this.Size.writeTo writer
            this.Color.writeTo writer

        static member readFrom (reader: System.IO.BinaryReader) =
            new Rect(Vec2Single.readFrom reader, Vec2Single.readFrom reader, 
                ColorFloat.readFrom reader)

    type Line(p1, p2, width, color) =
        inherit T()
        member this.P1 : Vec2Single.T = p1
        member this.P2 : Vec2Single.T = p2
        member this.Width : single = width
        member this.Color : ColorFloat.T = color

        override this.writeTo (writer: System.IO.BinaryWriter) = 
            writer.Write(int CustomDataType.Line)
            this.P1.writeTo writer
            this.P2.writeTo writer
            writer.Write this.Width
            this.Color.writeTo writer

        static member readFrom (reader: System.IO.BinaryReader) = 
            new Line(Vec2Single.readFrom reader, Vec2Single.readFrom reader, 
                reader.ReadSingle(), ColorFloat.readFrom reader)

    type Polygon(vertices) =
        inherit T()
        member this.Vertices: ColoredVertex.T[] = vertices

        override this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write(int CustomDataType.Polygon)
            writer.Write(this.Vertices.Length)
            this.Vertices |> Array.iter (fun i -> i.writeTo writer)

        static member readFrom (reader: System.IO.BinaryReader) = 
            new Polygon([|for _ = 1 to reader.ReadInt32() do yield ColoredVertex.readFrom reader|])
        
    type T with
        static member readFrom (reader: System.IO.BinaryReader) =
            match reader.ReadInt32() |> enum with
                | CustomDataType.Log -> Log.readFrom reader :> T
                | CustomDataType.Rect -> Rect.readFrom reader :> T
                | CustomDataType.Line -> Line.readFrom reader :> T
                | CustomDataType.Polygon -> Polygon.readFrom reader :> T
                | x -> failwith (sprintf "Unexpected CustomDataType %d" (int x))