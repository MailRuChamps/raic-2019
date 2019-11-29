namespace AiCup2019.Model

type Properties =
    {
        MaxTickCount: int
        TeamSize: int
        TicksPerSecond: double
        UpdatesPerTick: int
        LootBoxSize: Vec2Double
        UnitSize: Vec2Double
        UnitMaxHorizontalSpeed: double
        UnitFallSpeed: double
        UnitJumpTime: double
        UnitJumpSpeed: double
        JumpPadJumpTime: double
        JumpPadJumpSpeed: double
        UnitMaxHealth: int
        HealthPackHealth: int
        WeaponParameters: Map<WeaponType, WeaponParameters>
        MineSize: Vec2Double
        MineExplosionParameters: ExplosionParameters
        MinePrepareTime: double
        MineTriggerTime: double
        MineTriggerRadius: double
        KillScore: int
    } with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write this.MaxTickCount
        writer.Write this.TeamSize
        writer.Write this.UpdatesPerTick
        this.LootBoxSize.writeTo writer
        this.UnitSize.writeTo writer
        writer.Write this.UnitMaxHorizontalSpeed
        writer.Write this.UnitFallSpeed
        writer.Write this.UnitJumpTime
        writer.Write this.UnitJumpSpeed
        writer.Write this.JumpPadJumpTime
        writer.Write this.JumpPadJumpSpeed
        writer.Write this.UnitMaxHealth
        writer.Write this.HealthPackHealth
        writer.Write this.WeaponParameters.Count
        this.WeaponParameters |> Map.iter (fun k v -> 
                                                writer.Write (int k)
                                                v.writeTo writer)
        this.MineSize.writeTo writer
        this.MineExplosionParameters.writeTo writer
        writer.Write this.MinePrepareTime
        writer.Write this.MineTriggerTime
        writer.Write this.MineTriggerRadius
        writer.Write this.KillScore

    static member readFrom (reader: System.IO.BinaryReader) =
        {
            MaxTickCount = reader.ReadInt32()
            TeamSize = reader.ReadInt32()
            TicksPerSecond = reader.ReadDouble()
            UpdatesPerTick = reader.ReadInt32()
            LootBoxSize = Vec2Double.readFrom reader
            UnitSize = Vec2Double.readFrom reader
            UnitMaxHorizontalSpeed = reader.ReadDouble()
            UnitFallSpeed = reader.ReadDouble()
            UnitJumpTime = reader.ReadDouble()
            UnitJumpSpeed = reader.ReadDouble()
            JumpPadJumpTime = reader.ReadDouble()
            JumpPadJumpSpeed = reader.ReadDouble()
            UnitMaxHealth = reader.ReadInt32()
            HealthPackHealth = reader.ReadInt32()
            WeaponParameters = [for _ = 1 to reader.ReadInt32() do 
                                    let k = match reader.ReadInt32() with 
                                            | 0 -> WeaponType.Pistol
                                            | 1 -> WeaponType.AssaultRifle
                                            | 2 -> WeaponType.RocketLauncher
                                            | x -> failwith (sprintf "Unexpected Weapon type %d" x)
                                    let v = WeaponParameters.readFrom reader
                                    yield (k,v)] 
                                |> Map.ofList
            MineSize = Vec2Double.readFrom reader
            MineExplosionParameters = ExplosionParameters.readFrom reader
            MinePrepareTime = reader.ReadDouble()
            MineTriggerTime = reader.ReadDouble()
            MineTriggerRadius = reader.ReadDouble()
            KillScore = reader.ReadInt32()
        }
