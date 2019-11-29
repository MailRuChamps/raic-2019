namespace AiCup2019.Model

type Weapon =
    {
        Type: WeaponType
        Parameters: WeaponParameters
        Magazine: int
        WasShooting: bool
        Spread: double
        FireTimer: option<double>
        LastAngle: option<double>
        LastFireTick: option<int>
    } with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write (int this.Type)
        this.Parameters.writeTo writer
        writer.Write this.Magazine
        writer.Write this.WasShooting
        writer.Write this.Spread
        match this.FireTimer with
            | Some x -> writer.Write true
                        writer.Write x
            | None -> writer.Write false
        match this.LastAngle with
            | Some x -> writer.Write true
                        writer.Write x
            | None -> writer.Write false
        match this.LastFireTick with
            | Some x -> writer.Write true
                        writer.Write x
            | None -> writer.Write false

    static member readFrom (reader:System.IO.BinaryReader) =
        {
            Type = match reader.ReadInt32() with
                        | 0 -> WeaponType.Pistol
                        | 1 -> WeaponType.AssaultRifle
                        | 2 -> WeaponType.RocketLauncher
                        | x -> failwith (sprintf "Unexpected WeaponType %d" x)
            Parameters = WeaponParameters.readFrom reader
            Magazine = reader.ReadInt32()
            WasShooting = reader.ReadBoolean()
            Spread = reader.ReadDouble()
            FireTimer = match reader.ReadBoolean() with
                            | true -> Some(reader.ReadDouble())
                            | _ -> None
            LastAngle = match reader.ReadBoolean() with
                            | true -> Some(reader.ReadDouble())
                            | _ -> None
            LastFireTick = match reader.ReadBoolean() with
                            | true -> Some(reader.ReadInt32())
                            | _ -> None
        }
        