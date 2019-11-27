#include "ColoredVertex.hpp"

ColoredVertex::ColoredVertex() { }
ColoredVertex::ColoredVertex(Vec2Float position, ColorFloat color) : position(position), color(color) { }
ColoredVertex ColoredVertex::readFrom(InputStream& stream) {
    ColoredVertex result;
    result.position = Vec2Float::readFrom(stream);
    result.color = ColorFloat::readFrom(stream);
    return result;
}
void ColoredVertex::writeTo(OutputStream& stream) const {
    position.writeTo(stream);
    color.writeTo(stream);
}
std::string ColoredVertex::toString() const {
    return std::string("ColoredVertex") + "(" +
        position.toString() +
        color.toString() +
        ")";
}
