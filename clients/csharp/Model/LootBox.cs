namespace AiCup2019.Model
{
    public struct LootBox
    {
        public Model.Vec2Double Position { get; set; }
        public Model.Vec2Double Size { get; set; }
        public Model.Item Item { get; set; }
        public LootBox(Model.Vec2Double position, Model.Vec2Double size, Model.Item item)
        {
            this.Position = position;
            this.Size = size;
            this.Item = item;
        }
        public static LootBox ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new LootBox();
            result.Position = Model.Vec2Double.ReadFrom(reader);
            result.Size = Model.Vec2Double.ReadFrom(reader);
            result.Item = Model.Item.ReadFrom(reader);
            return result;
        }
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            Position.WriteTo(writer);
            Size.WriteTo(writer);
            Item.WriteTo(writer);
        }
    }
}
