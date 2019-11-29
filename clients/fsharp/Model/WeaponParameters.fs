namespace AiCup2019.Model

type WeaponParameters =
    {
        MagazineSize: int
        FireRate: double
        ReloadTime: double
        MinSpread: double
        MaxSpread: double
        Recoil: double
        AimSpeed: double
        Bullet: BulletParameters
        Explosion: option<ExplosionParameters>
    } with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write this.MagazineSize
        writer.Write this.FireRate
        writer.Write this.ReloadTime
        writer.Write this.MinSpread
        writer.Write this.MaxSpread
        writer.Write this.Recoil
        writer.Write this.AimSpeed
        this.Bullet.writeTo writer
        match this.Explosion with
            | Some x -> writer.Write true
                        x.writeTo writer
            | None -> writer.Write false
    
    static member readFrom (reader: System.IO.BinaryReader) =
        {
            MagazineSize = reader.ReadInt32()
            FireRate = reader.ReadDouble()
            ReloadTime = reader.ReadDouble()
            MinSpread = reader.ReadDouble()
            MaxSpread = reader.ReadDouble()
            Recoil = reader.ReadDouble()
            AimSpeed = reader.ReadDouble()
            Bullet = BulletParameters.readFrom reader
            Explosion = match reader.ReadBoolean() with
                            | true -> Some (ExplosionParameters.readFrom reader)
                            | _ -> None
        }
