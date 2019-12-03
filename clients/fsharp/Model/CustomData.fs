#nowarn "0058"
namespace AiCup2019.Model

type CustomDataLog = {
    Text: string;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 0
        let TextData : byte[] = System.Text.Encoding.UTF8.GetBytes this.Text
        writer.Write TextData.Length
        writer.Write TextData
    static member readFrom(reader: System.IO.BinaryReader) = {
        Text = reader.ReadInt32() |> reader.ReadBytes |> System.Text.Encoding.UTF8.GetString
    }

type CustomDataRect = {
    Pos: Vec2Single;
    Size: Vec2Single;
    Color: ColorSingle;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 1
        this.Pos.writeTo writer
        this.Size.writeTo writer
        this.Color.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) = {
        Pos = Vec2Single.readFrom reader
        Size = Vec2Single.readFrom reader
        Color = ColorSingle.readFrom reader
    }

type CustomDataLine = {
    P1: Vec2Single;
    P2: Vec2Single;
    Width: single;
    Color: ColorSingle;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 2
        this.P1.writeTo writer
        this.P2.writeTo writer
        writer.Write this.Width
        this.Color.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) = {
        P1 = Vec2Single.readFrom reader
        P2 = Vec2Single.readFrom reader
        Width = reader.ReadSingle()
        Color = ColorSingle.readFrom reader
    }

type CustomDataPolygon = {
    Vertices: ColoredVertex[];
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 3
        writer.Write this.Vertices.Length
        this.Vertices |> Array.iter (fun value ->
            value.writeTo writer
        )
    static member readFrom(reader: System.IO.BinaryReader) = {
        Vertices = [|for _ in 1 .. reader.ReadInt32() do
            yield ColoredVertex.readFrom reader
        |]
    }

type CustomDataPlacedText = {
    Text: string;
    Pos: Vec2Single;
    Alignment: TextAlignment;
    Size: single;
    Color: ColorSingle;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 4
        let TextData : byte[] = System.Text.Encoding.UTF8.GetBytes this.Text
        writer.Write TextData.Length
        writer.Write TextData
        this.Pos.writeTo writer
        writer.Write (int this.Alignment)
        writer.Write this.Size
        this.Color.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) = {
        Text = reader.ReadInt32() |> reader.ReadBytes |> System.Text.Encoding.UTF8.GetString
        Pos = Vec2Single.readFrom reader
        Alignment = reader.ReadInt32() |> enum
        Size = reader.ReadSingle()
        Color = ColorSingle.readFrom reader
    }
type CustomData = 
    | Log of CustomDataLog
    | Rect of CustomDataRect
    | Line of CustomDataLine
    | Polygon of CustomDataPolygon
    | PlacedText of CustomDataPlacedText
    with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this with
            | Log value -> value.writeTo writer
            | Rect value -> value.writeTo writer
            | Line value -> value.writeTo writer
            | Polygon value -> value.writeTo writer
            | PlacedText value -> value.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) =
        match reader.ReadInt32() with
            | 0 -> Log (CustomDataLog.readFrom reader)
            | 1 -> Rect (CustomDataRect.readFrom reader)
            | 2 -> Line (CustomDataLine.readFrom reader)
            | 3 -> Polygon (CustomDataPolygon.readFrom reader)
            | 4 -> PlacedText (CustomDataPlacedText.readFrom reader)
            | x -> failwith (sprintf "Unexpected CustomDataType %d" x)
