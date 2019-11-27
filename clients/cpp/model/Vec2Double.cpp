#include "Vec2Double.hpp"

Vec2Double::Vec2Double() { }
Vec2Double::Vec2Double(double x, double y) : x(x), y(y) { }
Vec2Double Vec2Double::readFrom(InputStream& stream) {
    Vec2Double result;
    result.x = stream.readDouble();
    result.y = stream.readDouble();
    return result;
}
void Vec2Double::writeTo(OutputStream& stream) const {
    stream.write(x);
    stream.write(y);
}
std::string Vec2Double::toString() const {
    return std::string("Vec2Double") + "(" +
        std::to_string(x) +
        std::to_string(y) +
        ")";
}
