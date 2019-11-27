import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct ColoredVertex {
    Vec2Float position;
    ColorFloat color;
    this(Vec2Float position, ColorFloat color) {
        this.position = position;
        this.color = color;
    }
    static ColoredVertex readFrom(Stream reader) {
        auto result = ColoredVertex();
        result.position = Vec2Float.readFrom(reader);
        result.color = ColorFloat.readFrom(reader);
        return result;
    }
    void writeTo(Stream writer) const {
        position.writeTo(writer);
        color.writeTo(writer);
    }
    string toString() const {
        return "ColoredVertex" ~ "(" ~
            to!string(position) ~
            to!string(color) ~
            ")";
    }
}
