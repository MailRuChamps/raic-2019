#nowarn "0058"
namespace AiCup2019.Model
type Unit = {
    PlayerId: int;
    Id: int;
    Health: int;
    Position: Vec2Double;
    Size: Vec2Double;
    JumpState: JumpState;
    WalkedRight: bool;
    Stand: bool;
    OnGround: bool;
    OnLadder: bool;
    Mines: int;
    Weapon: option<Weapon>;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.PlayerId
        writer.Write this.Id
        writer.Write this.Health
        this.Position.writeTo writer
        this.Size.writeTo writer
        this.JumpState.writeTo writer
        writer.Write this.WalkedRight
        writer.Write this.Stand
        writer.Write this.OnGround
        writer.Write this.OnLadder
        writer.Write this.Mines
        match this.Weapon with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
    static member readFrom(reader: System.IO.BinaryReader) = {
        PlayerId = reader.ReadInt32()
        Id = reader.ReadInt32()
        Health = reader.ReadInt32()
        Position = Vec2Double.readFrom reader
        Size = Vec2Double.readFrom reader
        JumpState = JumpState.readFrom reader
        WalkedRight = reader.ReadBoolean()
        Stand = reader.ReadBoolean()
        OnGround = reader.ReadBoolean()
        OnLadder = reader.ReadBoolean()
        Mines = reader.ReadInt32()
        Weapon = match reader.ReadBoolean() with
            | true ->
                Some(
                    Weapon.readFrom reader
                    )
            | false -> None
    }
