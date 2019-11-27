namespace AiCup2019.Model
{
    public struct Player
    {
        public int Id { get; set; }
        public int Score { get; set; }
        public Player(int id, int score)
        {
            this.Id = id;
            this.Score = score;
        }
        public static Player ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Player();
            result.Id = reader.ReadInt32();
            result.Score = reader.ReadInt32();
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(Id);
            writer.Write(Score);
        }
    }
}
