#nowarn "0058"
namespace AiCup2019.Model
type Game = {
    CurrentTick: int;
    Properties: Properties;
    Level: Level;
    Players: Player[];
    Units: Unit[];
    Bullets: Bullet[];
    Mines: Mine[];
    LootBoxes: LootBox[];
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.CurrentTick
        this.Properties.writeTo writer
        this.Level.writeTo writer
        writer.Write this.Players.Length
        this.Players |> Array.iter (fun value ->
            value.writeTo writer
        )
        writer.Write this.Units.Length
        this.Units |> Array.iter (fun value ->
            value.writeTo writer
        )
        writer.Write this.Bullets.Length
        this.Bullets |> Array.iter (fun value ->
            value.writeTo writer
        )
        writer.Write this.Mines.Length
        this.Mines |> Array.iter (fun value ->
            value.writeTo writer
        )
        writer.Write this.LootBoxes.Length
        this.LootBoxes |> Array.iter (fun value ->
            value.writeTo writer
        )
    static member readFrom(reader: System.IO.BinaryReader) = {
        CurrentTick = reader.ReadInt32()
        Properties = Properties.readFrom reader
        Level = Level.readFrom reader
        Players = [|for _ in 1 .. reader.ReadInt32() do
            yield Player.readFrom reader
        |]
        Units = [|for _ in 1 .. reader.ReadInt32() do
            yield Unit.readFrom reader
        |]
        Bullets = [|for _ in 1 .. reader.ReadInt32() do
            yield Bullet.readFrom reader
        |]
        Mines = [|for _ in 1 .. reader.ReadInt32() do
            yield Mine.readFrom reader
        |]
        LootBoxes = [|for _ in 1 .. reader.ReadInt32() do
            yield LootBox.readFrom reader
        |]
    }
