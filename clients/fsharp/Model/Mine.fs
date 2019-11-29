namespace AiCup2019.Model

type Mine = 
    {
        PlayerId: int
        Position: Vec2Double
        Size: Vec2Double
        State: MineState
        Timer: option<double>
        TriggerRadius: double
        ExplosionParameters: ExplosionParameters
    } with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write this.PlayerId
        this.Position.writeTo writer
        this.Size.writeTo writer
        writer.Write (int this.State)
        match this.Timer with
            | Some x -> writer.Write true
                        writer.Write x
            | None -> writer.Write false
        writer.Write this.TriggerRadius
        this.ExplosionParameters.writeTo writer

    static member readFrom (reader: System.IO.BinaryReader) =
        {
            PlayerId = reader.ReadInt32()
            Position = Vec2Double.readFrom reader
            Size = Vec2Double.readFrom reader
            State = match reader.ReadInt32() with
                        | 0 -> MineState.Preparing
                        | 1 -> MineState.Idle
                        | 2 -> MineState.Triggered
                        | 3 -> MineState.Exploded
                        | x -> failwith (sprintf "Unexpected MineState %d" x)
            Timer = match reader.ReadBoolean() with
                        | true -> Some(reader.ReadDouble())
                        | _ -> None
            TriggerRadius = reader.ReadDouble()
            ExplosionParameters = ExplosionParameters.readFrom reader
        }
