namespace AiCup2019.Model

type Bullet =
    {
        WeaponType: WeaponType
        UnitId: int
        PlayerId: int
        Position: Vec2Double
        Velocity: Vec2Double
        Damage: int
        Size: double
        ExplosionParameters: option<ExplosionParameters>
    } with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        int this.WeaponType |> writer.Write 
        writer.Write this.UnitId 
        writer.Write this.PlayerId
        this.Position.writeTo writer
        this.Velocity.writeTo writer
        writer.Write this.Damage
        writer.Write this.Size
        match this.ExplosionParameters with
            | Some x -> writer.Write true
                        x.writeTo writer
            | None -> writer.Write false

    static member readFrom (reader: System.IO.BinaryReader) =
        let weaponType = match reader.ReadInt32() with
                            | 0 -> WeaponType.Pistol
                            | 1 -> WeaponType.AssaultRifle
                            | 2 -> WeaponType.RocketLauncher
                            | x -> failwith (sprintf "Unexpected WeaponType %d" x)         
        {
            WeaponType = weaponType
            UnitId = reader.ReadInt32()
            PlayerId = reader.ReadInt32()
            Position = Vec2Double.readFrom reader
            Velocity = Vec2Double.readFrom reader
            Damage = reader.ReadInt32()
            Size = reader.ReadDouble()
            ExplosionParameters = match reader.ReadBoolean() with
                                    | true -> Some(ExplosionParameters.readFrom reader)
                                    | _ -> None
        }
