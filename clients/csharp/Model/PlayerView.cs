namespace AiCup2019.Model
{
    public struct PlayerView
    {
        public int MyId { get; set; }
        public Model.Game Game { get; set; }
        public PlayerView(int myId, Model.Game game)
        {
            this.MyId = myId;
            this.Game = game;
        }
        public static PlayerView ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new PlayerView();
            result.MyId = reader.ReadInt32();
            result.Game = Model.Game.ReadFrom(reader);
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(MyId);
            Game.WriteTo(writer);
        }
    }
}
