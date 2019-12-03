#nowarn "0058"
namespace AiCup2019.Model
type Bullet = {
    WeaponType: WeaponType;
    UnitId: int;
    PlayerId: int;
    Position: Vec2Double;
    Velocity: Vec2Double;
    Damage: int;
    Size: double;
    ExplosionParameters: option<ExplosionParameters>;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write (int this.WeaponType)
        writer.Write this.UnitId
        writer.Write this.PlayerId
        this.Position.writeTo writer
        this.Velocity.writeTo writer
        writer.Write this.Damage
        writer.Write this.Size
        match this.ExplosionParameters with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
    static member readFrom(reader: System.IO.BinaryReader) = {
        WeaponType = reader.ReadInt32() |> enum
        UnitId = reader.ReadInt32()
        PlayerId = reader.ReadInt32()
        Position = Vec2Double.readFrom reader
        Velocity = Vec2Double.readFrom reader
        Damage = reader.ReadInt32()
        Size = reader.ReadDouble()
        ExplosionParameters = match reader.ReadBoolean() with
            | true ->
                Some(
                    ExplosionParameters.readFrom reader
                    )
            | false -> None
    }
