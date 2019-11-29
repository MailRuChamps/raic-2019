namespace AiCup2019.Model

type ItemType = HealthPack = 0 | Weapon = 1 | Mine = 2

module Item = 
    type HealthPackItem = {Health: int} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write (int ItemType.HealthPack)
            writer.Write this.Health
        
        static member readFrom (reader: System.IO.BinaryReader) = { Health = reader.ReadInt32()}

    type WeaponItem = {WeaponType: WeaponType} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write (int ItemType.Weapon)
            writer.Write (int this.WeaponType)
        
        static member readFrom (reader: System.IO.BinaryReader) =
            { 
                WeaponType = match reader.ReadInt32() with
                                | 0 -> WeaponType.Pistol
                                | 1 -> WeaponType.AssaultRifle
                                | 2 -> WeaponType.RocketLauncher
                                | x -> failwith (sprintf "Unexpected WeaponType %d" x)
            }

    type MineItem = struct end with
        member this.writeTo (writer: System.IO.BinaryWriter) = writer.Write (int ItemType.Mine)

        static member readFrom (reader: System.IO.BinaryReader) = new MineItem()
    
    type T = HealthPack of HealthPackItem | WeaponItem of WeaponItem | Mine of MineItem with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            match this with
                | HealthPack x -> x.writeTo writer
                | WeaponItem x -> x.writeTo writer
                | Mine x -> x.writeTo writer

    let readFrom (reader: System.IO.BinaryReader)  =
        match reader.ReadInt32() |> enum with
            | ItemType.HealthPack -> HealthPack (HealthPackItem.readFrom reader)
            | ItemType.Weapon -> WeaponItem (WeaponItem.readFrom reader)
            | ItemType.Mine -> Mine (MineItem.readFrom reader)
            | x -> failwith (sprintf "Unexpected ItemType %d" (int x))
