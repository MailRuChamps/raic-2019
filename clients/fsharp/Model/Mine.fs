#nowarn "0058"
namespace AiCup2019.Model
type Mine = {
    PlayerId: int;
    Position: Vec2Double;
    Size: Vec2Double;
    State: MineState;
    Timer: option<double>;
    TriggerRadius: double;
    ExplosionParameters: ExplosionParameters;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.PlayerId
        this.Position.writeTo writer
        this.Size.writeTo writer
        writer.Write (int this.State)
        match this.Timer with
            | Some value ->
                writer.Write true
                writer.Write value
            | None -> writer.Write false
        writer.Write this.TriggerRadius
        this.ExplosionParameters.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) = {
        PlayerId = reader.ReadInt32()
        Position = Vec2Double.readFrom reader
        Size = Vec2Double.readFrom reader
        State = reader.ReadInt32() |> enum
        Timer = match reader.ReadBoolean() with
            | true ->
                Some(
                    reader.ReadDouble()
                    )
            | false -> None
        TriggerRadius = reader.ReadDouble()
        ExplosionParameters = ExplosionParameters.readFrom reader
    }
