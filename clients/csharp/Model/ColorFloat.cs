namespace AiCup2019.Model
{
    public struct ColorFloat
    {
        public float R { get; set; }
        public float G { get; set; }
        public float B { get; set; }
        public float A { get; set; }
        public ColorFloat(float r, float g, float b, float a)
        {
            this.R = r;
            this.G = g;
            this.B = b;
            this.A = a;
        }
        public static ColorFloat ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new ColorFloat();
            result.R = reader.ReadSingle();
            result.G = reader.ReadSingle();
            result.B = reader.ReadSingle();
            result.A = reader.ReadSingle();
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(R);
            writer.Write(G);
            writer.Write(B);
            writer.Write(A);
        }
    }
}
