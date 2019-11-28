namespace AiCup2019.Model

type ItemType = HealthPack = 0 | Weapon = 1 | Mine = 2

module Item = 
    [<AbstractClass>]
    type T() =
        abstract member writeTo: System.IO.BinaryWriter -> unit

    type HealthPackItem(health) =
        inherit T()
        member this.Health : int = health

        override this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write (int ItemType.HealthPack)
            writer.Write this.Health

        static member readFrom (reader: System.IO.BinaryReader) = new HealthPackItem(reader.ReadInt32())

    type WeaponItem(weaponType) =
        inherit T()
        member this.WeaponType : WeaponType = weaponType

        override this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write (int ItemType.Weapon)
            writer.Write (int this.WeaponType)

        static member readFrom (reader: System.IO.BinaryReader) =
            new WeaponItem(match reader.ReadInt32() with
                        | 0 -> WeaponType.Pistol
                        | 1 -> WeaponType.AssaultRifle
                        | 2 -> WeaponType.RocketLauncher
                        | x -> failwith (sprintf "Unexpected WeaponType %d" x))

    type MineItem() =
        inherit T()

        override this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write (int ItemType.Mine)

        static member readFrom (reader: System.IO.BinaryReader) = new MineItem()

    type T with
        static member readFrom (reader: System.IO.BinaryReader) =
            match reader.ReadInt32() |> enum with
                | ItemType.HealthPack -> HealthPackItem.readFrom reader :> T
                | ItemType.Weapon -> WeaponItem.readFrom reader :> T
                | ItemType.Mine -> MineItem.readFrom reader :> T
                | x -> failwith (sprintf "Unexpected ItemType %d" (int x))
    


