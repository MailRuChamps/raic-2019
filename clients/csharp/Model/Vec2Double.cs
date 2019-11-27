namespace AiCup2019.Model
{
    public struct Vec2Double
    {
        public double X { get; set; }
        public double Y { get; set; }
        public Vec2Double(double x, double y)
        {
            this.X = x;
            this.Y = y;
        }
        public static Vec2Double ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Vec2Double();
            result.X = reader.ReadDouble();
            result.Y = reader.ReadDouble();
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(X);
            writer.Write(Y);
        }
    }
}
