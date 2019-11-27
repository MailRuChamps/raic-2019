namespace AiCup2019.Model
{
    public struct ColoredVertex
    {
        public Model.Vec2Float Position { get; set; }
        public Model.ColorFloat Color { get; set; }
        public ColoredVertex(Model.Vec2Float position, Model.ColorFloat color)
        {
            this.Position = position;
            this.Color = color;
        }
        public static ColoredVertex ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new ColoredVertex();
            result.Position = Model.Vec2Float.ReadFrom(reader);
            result.Color = Model.ColorFloat.ReadFrom(reader);
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            Position.WriteTo(writer);
            Color.WriteTo(writer);
        }
    }
}
