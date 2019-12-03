#nowarn "0058"
namespace AiCup2019.Model
type UnitAction = {
    Velocity: double;
    Jump: bool;
    JumpDown: bool;
    Aim: Vec2Double;
    Shoot: bool;
    Reload: bool;
    SwapWeapon: bool;
    PlantMine: bool;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.Velocity
        writer.Write this.Jump
        writer.Write this.JumpDown
        this.Aim.writeTo writer
        writer.Write this.Shoot
        writer.Write this.Reload
        writer.Write this.SwapWeapon
        writer.Write this.PlantMine
    static member readFrom(reader: System.IO.BinaryReader) = {
        Velocity = reader.ReadDouble()
        Jump = reader.ReadBoolean()
        JumpDown = reader.ReadBoolean()
        Aim = Vec2Double.readFrom reader
        Shoot = reader.ReadBoolean()
        Reload = reader.ReadBoolean()
        SwapWeapon = reader.ReadBoolean()
        PlantMine = reader.ReadBoolean()
    }
