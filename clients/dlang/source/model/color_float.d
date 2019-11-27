import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct ColorFloat {
    float r;
    float g;
    float b;
    float a;
    this(float r, float g, float b, float a) {
        this.r = r;
        this.g = g;
        this.b = b;
        this.a = a;
    }
    static ColorFloat readFrom(Stream reader) {
        auto result = ColorFloat();
        result.r = reader.readFloat();
        result.g = reader.readFloat();
        result.b = reader.readFloat();
        result.a = reader.readFloat();
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(r);
        writer.write(g);
        writer.write(b);
        writer.write(a);
    }
    string toString() const {
        return "ColorFloat" ~ "(" ~
            to!string(r) ~
            to!string(g) ~
            to!string(b) ~
            to!string(a) ~
            ")";
    }
}
