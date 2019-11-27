import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct ExplosionParameters {
    double radius;
    int damage;
    this(double radius, int damage) {
        this.radius = radius;
        this.damage = damage;
    }
    static ExplosionParameters readFrom(Stream reader) {
        auto result = ExplosionParameters();
        result.radius = reader.readDouble();
        result.damage = reader.readInt();
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(radius);
        writer.write(damage);
    }
    string toString() const {
        return "ExplosionParameters" ~ "(" ~
            to!string(radius) ~
            to!string(damage) ~
            ")";
    }
}
