#include "BulletParams.hpp"

BulletParams::BulletParams() { }
BulletParams::BulletParams(double speed, double size, int damage) : speed(speed), size(size), damage(damage) { }
BulletParams BulletParams::readFrom(InputStream& stream) {
    BulletParams result;
    result.speed = stream.readDouble();
    result.size = stream.readDouble();
    result.damage = stream.readInt();
    return result;
}
void BulletParams::writeTo(OutputStream& stream) const {
    stream.write(speed);
    stream.write(size);
    stream.write(damage);
}
std::string BulletParams::toString() const {
    return std::string("BulletParams") + "(" +
        std::to_string(speed) +
        std::to_string(size) +
        std::to_string(damage) +
        ")";
}
