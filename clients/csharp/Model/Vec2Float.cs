namespace AiCup2019.Model
{
    public struct Vec2Float
    {
        public float X { get; set; }
        public float Y { get; set; }
        public Vec2Float(float x, float y)
        {
            this.X = x;
            this.Y = y;
        }
        public static Vec2Float ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Vec2Float();
            result.X = reader.ReadSingle();
            result.Y = reader.ReadSingle();
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(X);
            writer.Write(Y);
        }
    }
}
