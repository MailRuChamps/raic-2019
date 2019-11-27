import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct BulletParameters {
    double speed;
    double size;
    int damage;
    this(double speed, double size, int damage) {
        this.speed = speed;
        this.size = size;
        this.damage = damage;
    }
    static BulletParameters readFrom(Stream reader) {
        auto result = BulletParameters();
        result.speed = reader.readDouble();
        result.size = reader.readDouble();
        result.damage = reader.readInt();
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(speed);
        writer.write(size);
        writer.write(damage);
    }
    string toString() const {
        return "BulletParameters" ~ "(" ~
            to!string(speed) ~
            to!string(size) ~
            to!string(damage) ~
            ")";
    }
}
