import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Vec2Double {
    double x;
    double y;
    this(double x, double y) {
        this.x = x;
        this.y = y;
    }
    static Vec2Double readFrom(Stream reader) {
        auto result = Vec2Double();
        result.x = reader.readDouble();
        result.y = reader.readDouble();
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(x);
        writer.write(y);
    }
    string toString() const {
        return "Vec2Double" ~ "(" ~
            to!string(x) ~
            to!string(y) ~
            ")";
    }
}
