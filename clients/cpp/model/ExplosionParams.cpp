#include "ExplosionParams.hpp"

ExplosionParams::ExplosionParams() { }
ExplosionParams::ExplosionParams(double radius, int damage) : radius(radius), damage(damage) { }
ExplosionParams ExplosionParams::readFrom(InputStream& stream) {
    ExplosionParams result;
    result.radius = stream.readDouble();
    result.damage = stream.readInt();
    return result;
}
void ExplosionParams::writeTo(OutputStream& stream) const {
    stream.write(radius);
    stream.write(damage);
}
std::string ExplosionParams::toString() const {
    return std::string("ExplosionParams") + "(" +
        std::to_string(radius) +
        std::to_string(damage) +
        ")";
}
