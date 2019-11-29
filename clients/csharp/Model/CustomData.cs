namespace AiCup2019.Model
{
    public abstract class CustomData
    {
        public abstract void WriteTo(System.IO.BinaryWriter writer);
        public static CustomData ReadFrom(System.IO.BinaryReader reader)
        {
            switch (reader.ReadInt32())
            {
                case Log.TAG:
                    return Log.ReadFrom(reader);
                case Rect.TAG:
                    return Rect.ReadFrom(reader);
                case Line.TAG:
                    return Line.ReadFrom(reader);
                case Polygon.TAG:
                    return Polygon.ReadFrom(reader);
                case PlacedText.TAG:
                    return PlacedText.ReadFrom(reader);
                default:
                    throw new System.Exception("Unexpected discriminant value");
            }
        }

        public class Log : CustomData
        {
            public const int TAG = 0;
            public string Text { get; set; }
            public Log() {}
            public Log(string text)
            {
                this.Text = text;
            }
            public static new Log ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Log();
                result.Text = System.Text.Encoding.UTF8.GetString(reader.ReadBytes(reader.ReadInt32()));
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                var TextData = System.Text.Encoding.UTF8.GetBytes(Text);
                writer.Write(TextData.Length);
                writer.Write(TextData);
            }
        }

        public class Rect : CustomData
        {
            public const int TAG = 1;
            public Model.Vec2Float Pos { get; set; }
            public Model.Vec2Float Size { get; set; }
            public Model.ColorFloat Color { get; set; }
            public Rect() {}
            public Rect(Model.Vec2Float pos, Model.Vec2Float size, Model.ColorFloat color)
            {
                this.Pos = pos;
                this.Size = size;
                this.Color = color;
            }
            public static new Rect ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Rect();
                result.Pos = Model.Vec2Float.ReadFrom(reader);
                result.Size = Model.Vec2Float.ReadFrom(reader);
                result.Color = Model.ColorFloat.ReadFrom(reader);
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                Pos.WriteTo(writer);
                Size.WriteTo(writer);
                Color.WriteTo(writer);
            }
        }

        public class Line : CustomData
        {
            public const int TAG = 2;
            public Model.Vec2Float P1 { get; set; }
            public Model.Vec2Float P2 { get; set; }
            public float Width { get; set; }
            public Model.ColorFloat Color { get; set; }
            public Line() {}
            public Line(Model.Vec2Float p1, Model.Vec2Float p2, float width, Model.ColorFloat color)
            {
                this.P1 = p1;
                this.P2 = p2;
                this.Width = width;
                this.Color = color;
            }
            public static new Line ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Line();
                result.P1 = Model.Vec2Float.ReadFrom(reader);
                result.P2 = Model.Vec2Float.ReadFrom(reader);
                result.Width = reader.ReadSingle();
                result.Color = Model.ColorFloat.ReadFrom(reader);
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                P1.WriteTo(writer);
                P2.WriteTo(writer);
                writer.Write(Width);
                Color.WriteTo(writer);
            }
        }

        public class Polygon : CustomData
        {
            public const int TAG = 3;
            public Model.ColoredVertex[] Vertices { get; set; }
            public Polygon() {}
            public Polygon(Model.ColoredVertex[] vertices)
            {
                this.Vertices = vertices;
            }
            public static new Polygon ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Polygon();
                result.Vertices = new Model.ColoredVertex[reader.ReadInt32()];
                for (int i = 0; i < result.Vertices.Length; i++)
                {
                    result.Vertices[i] = Model.ColoredVertex.ReadFrom(reader);
                }
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                writer.Write(Vertices.Length);
                foreach (var VerticesElement in Vertices)
                {
                    VerticesElement.WriteTo(writer);
                }
            }
        }

        public class PlacedText : CustomData
        {
            public const int TAG = 4;
            public string Text { get; set; }
            public Model.Vec2Float Pos { get; set; }
            public Model.TextAlignment Alignment { get; set; }
            public float Size { get; set; }
            public Model.ColorFloat Color { get; set; }
            public PlacedText() {}
            public PlacedText(string text, Model.Vec2Float pos, Model.TextAlignment alignment, float size, Model.ColorFloat color)
            {
                this.Text = text;
                this.Pos = pos;
                this.Alignment = alignment;
                this.Size = size;
                this.Color = color;
            }
            public static new PlacedText ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new PlacedText();
                result.Text = System.Text.Encoding.UTF8.GetString(reader.ReadBytes(reader.ReadInt32()));
                result.Pos = Model.Vec2Float.ReadFrom(reader);
                switch (reader.ReadInt32())
                {
                case 0:
                    result.Alignment = Model.TextAlignment.Left;
                    break;
                case 1:
                    result.Alignment = Model.TextAlignment.Center;
                    break;
                case 2:
                    result.Alignment = Model.TextAlignment.Right;
                    break;
                default:
                    throw new System.Exception("Unexpected discriminant value");
                }
                result.Size = reader.ReadSingle();
                result.Color = Model.ColorFloat.ReadFrom(reader);
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                var TextData = System.Text.Encoding.UTF8.GetBytes(Text);
                writer.Write(TextData.Length);
                writer.Write(TextData);
                Pos.WriteTo(writer);
                writer.Write((int) (Alignment));
                writer.Write(Size);
                Color.WriteTo(writer);
            }
        }
    }
}
