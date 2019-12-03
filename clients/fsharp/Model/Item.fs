#nowarn "0058"
namespace AiCup2019.Model

type ItemHealthPack = {
    Health: int;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 0
        writer.Write this.Health
    static member readFrom(reader: System.IO.BinaryReader) = {
        Health = reader.ReadInt32()
    }

type ItemWeapon = {
    WeaponType: WeaponType;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 1
        writer.Write (int this.WeaponType)
    static member readFrom(reader: System.IO.BinaryReader) = {
        WeaponType = reader.ReadInt32() |> enum
    }

type ItemMine = struct end with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 2
    static member readFrom(reader: System.IO.BinaryReader) = new ItemMine()
type Item = 
    | HealthPack of ItemHealthPack
    | Weapon of ItemWeapon
    | Mine of ItemMine
    with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this with
            | HealthPack value -> value.writeTo writer
            | Weapon value -> value.writeTo writer
            | Mine value -> value.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) =
        match reader.ReadInt32() with
            | 0 -> HealthPack (ItemHealthPack.readFrom reader)
            | 1 -> Weapon (ItemWeapon.readFrom reader)
            | 2 -> Mine (ItemMine.readFrom reader)
            | x -> failwith (sprintf "Unexpected CustomDataType %d" x)
