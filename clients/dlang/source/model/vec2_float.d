import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Vec2Float {
    float x;
    float y;
    this(float x, float y) {
        this.x = x;
        this.y = y;
    }
    static Vec2Float readFrom(Stream reader) {
        auto result = Vec2Float();
        result.x = reader.readFloat();
        result.y = reader.readFloat();
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(x);
        writer.write(y);
    }
    string toString() const {
        return "Vec2Float" ~ "(" ~
            to!string(x) ~
            to!string(y) ~
            ")";
    }
}
