namespace AiCup2019.Model
{
    public struct Game
    {
        public int CurrentTick { get; set; }
        public Model.Properties Properties { get; set; }
        public Model.Level Level { get; set; }
        public Model.Player[] Players { get; set; }
        public Model.Unit[] Units { get; set; }
        public Model.Bullet[] Bullets { get; set; }
        public Model.Mine[] Mines { get; set; }
        public Model.LootBox[] LootBoxes { get; set; }
        public Game(int currentTick, Model.Properties properties, Model.Level level, Model.Player[] players, Model.Unit[] units, Model.Bullet[] bullets, Model.Mine[] mines, Model.LootBox[] lootBoxes)
        {
            this.CurrentTick = currentTick;
            this.Properties = properties;
            this.Level = level;
            this.Players = players;
            this.Units = units;
            this.Bullets = bullets;
            this.Mines = mines;
            this.LootBoxes = lootBoxes;
        }
        public static Game ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Game();
            result.CurrentTick = reader.ReadInt32();
            result.Properties = Model.Properties.ReadFrom(reader);
            result.Level = Model.Level.ReadFrom(reader);
            result.Players = new Model.Player[reader.ReadInt32()];
            for (int i = 0; i < result.Players.Length; i++)
            {
                result.Players[i] = Model.Player.ReadFrom(reader);
            }
            result.Units = new Model.Unit[reader.ReadInt32()];
            for (int i = 0; i < result.Units.Length; i++)
            {
                result.Units[i] = Model.Unit.ReadFrom(reader);
            }
            result.Bullets = new Model.Bullet[reader.ReadInt32()];
            for (int i = 0; i < result.Bullets.Length; i++)
            {
                result.Bullets[i] = Model.Bullet.ReadFrom(reader);
            }
            result.Mines = new Model.Mine[reader.ReadInt32()];
            for (int i = 0; i < result.Mines.Length; i++)
            {
                result.Mines[i] = Model.Mine.ReadFrom(reader);
            }
            result.LootBoxes = new Model.LootBox[reader.ReadInt32()];
            for (int i = 0; i < result.LootBoxes.Length; i++)
            {
                result.LootBoxes[i] = Model.LootBox.ReadFrom(reader);
            }
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(CurrentTick);
            Properties.WriteTo(writer);
            Level.WriteTo(writer);
            writer.Write(Players.Length);
            foreach (var PlayersElement in Players)
            {
                PlayersElement.WriteTo(writer);
            }
            writer.Write(Units.Length);
            foreach (var UnitsElement in Units)
            {
                UnitsElement.WriteTo(writer);
            }
            writer.Write(Bullets.Length);
            foreach (var BulletsElement in Bullets)
            {
                BulletsElement.WriteTo(writer);
            }
            writer.Write(Mines.Length);
            foreach (var MinesElement in Mines)
            {
                MinesElement.WriteTo(writer);
            }
            writer.Write(LootBoxes.Length);
            foreach (var LootBoxesElement in LootBoxes)
            {
                LootBoxesElement.WriteTo(writer);
            }
        }
    }
}
