namespace AiCup2019.Model

module Game =
    type T =
        {
            CurrentTick: int
            Properties: Properties.T
            Level: Level.T
            Players: Player.T[]
            Units: Unit.T[]
            Bullets: Bullet.T[]
            Mines: Mine.T[]
            LootBoxes: LootBox.T[]
        } with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write this.CurrentTick
            this.Properties.writeTo writer
            this.Level.writeTo writer
            writer.Write this.Players.Length
            this.Players |> Array.iter (fun p -> p.writeTo writer)
            writer.Write this.Units.Length
            this.Units |> Array.iter (fun u -> u.writeTo writer)
            writer.Write this.Bullets.Length
            this.Bullets |> Array.iter (fun b -> b.writeTo writer)
            writer.Write this.Mines.Length
            this.Mines |> Array.iter (fun m -> m.writeTo writer)
            writer.Write this.LootBoxes.Length
            this.LootBoxes |> Array.iter (fun l -> l.writeTo writer)

    let readFrom (reader: System.IO.BinaryReader) =
        {
            CurrentTick = reader.ReadInt32()
            Properties = Properties.readFrom reader
            Level = Level.readFrom reader
            Players = [|for _ = 1 to reader.ReadInt32() do yield Player.readFrom reader|]
            Units = [|for _ = 1 to reader.ReadInt32() do yield Unit.readFrom reader|]
            Bullets = [|for _ = 1 to reader.ReadInt32() do yield Bullet.readFrom reader|]
            Mines = [|for _ = 1 to reader.ReadInt32() do yield Mine.readFrom reader|]
            LootBoxes = [|for _ = 1 to reader.ReadInt32() do yield LootBox.readFrom reader|]
        }


