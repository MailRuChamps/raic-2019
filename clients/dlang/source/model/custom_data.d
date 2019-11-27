import model;
import stream;
import std.conv;
import std.typecons : Nullable;

abstract class CustomData {
    abstract void writeTo(Stream writer) const;
    static CustomData readFrom(Stream reader) {
        switch (reader.readInt()) {
            case Log.TAG:
                return Log.readFrom(reader);
            case Rect.TAG:
                return Rect.readFrom(reader);
            case Line.TAG:
                return Line.readFrom(reader);
            case Polygon.TAG:
                return Polygon.readFrom(reader);
            default:
                throw new Exception("Unexpected discriminant value");
        }
    }

    static class Log : CustomData {
        static const int TAG = 0;
        string text;
        this() {}
        this(string text) {
            this.text = text;
        }
        static Log readFrom(Stream reader) {
            auto result = new Log();
            result.text = reader.readString();
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(text);
        }
        override string toString() const {
            return "Log" ~ "(" ~
                to!string(text) ~
                ")";
        }
    }

    static class Rect : CustomData {
        static const int TAG = 1;
        Vec2Float pos;
        Vec2Float size;
        ColorFloat color;
        this() {}
        this(Vec2Float pos, Vec2Float size, ColorFloat color) {
            this.pos = pos;
            this.size = size;
            this.color = color;
        }
        static Rect readFrom(Stream reader) {
            auto result = new Rect();
            result.pos = Vec2Float.readFrom(reader);
            result.size = Vec2Float.readFrom(reader);
            result.color = ColorFloat.readFrom(reader);
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            pos.writeTo(writer);
            size.writeTo(writer);
            color.writeTo(writer);
        }
        override string toString() const {
            return "Rect" ~ "(" ~
                to!string(pos) ~
                to!string(size) ~
                to!string(color) ~
                ")";
        }
    }

    static class Line : CustomData {
        static const int TAG = 2;
        Vec2Float p1;
        Vec2Float p2;
        float width;
        ColorFloat color;
        this() {}
        this(Vec2Float p1, Vec2Float p2, float width, ColorFloat color) {
            this.p1 = p1;
            this.p2 = p2;
            this.width = width;
            this.color = color;
        }
        static Line readFrom(Stream reader) {
            auto result = new Line();
            result.p1 = Vec2Float.readFrom(reader);
            result.p2 = Vec2Float.readFrom(reader);
            result.width = reader.readFloat();
            result.color = ColorFloat.readFrom(reader);
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            p1.writeTo(writer);
            p2.writeTo(writer);
            writer.write(width);
            color.writeTo(writer);
        }
        override string toString() const {
            return "Line" ~ "(" ~
                to!string(p1) ~
                to!string(p2) ~
                to!string(width) ~
                to!string(color) ~
                ")";
        }
    }

    static class Polygon : CustomData {
        static const int TAG = 3;
        ColoredVertex[] vertices;
        this() {}
        this(ColoredVertex[] vertices) {
            this.vertices = vertices;
        }
        static Polygon readFrom(Stream reader) {
            auto result = new Polygon();
            result.vertices = new ColoredVertex[reader.readInt()];
            for (int i = 0; i < result.vertices.length; i++) {
                result.vertices[i] = ColoredVertex.readFrom(reader);
            }
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(cast(int)(vertices.length));
            foreach (verticesElement; vertices) {
                verticesElement.writeTo(writer);
            }
        }
        override string toString() const {
            return "Polygon" ~ "(" ~
                to!string(vertices) ~
                ")";
        }
    }
}
