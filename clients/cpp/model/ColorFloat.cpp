#include "ColorFloat.hpp"

ColorFloat::ColorFloat() { }
ColorFloat::ColorFloat(float r, float g, float b, float a) : r(r), g(g), b(b), a(a) { }
ColorFloat ColorFloat::readFrom(InputStream& stream) {
    ColorFloat result;
    result.r = stream.readFloat();
    result.g = stream.readFloat();
    result.b = stream.readFloat();
    result.a = stream.readFloat();
    return result;
}
void ColorFloat::writeTo(OutputStream& stream) const {
    stream.write(r);
    stream.write(g);
    stream.write(b);
    stream.write(a);
}
std::string ColorFloat::toString() const {
    return std::string("ColorFloat") + "(" +
        std::to_string(r) +
        std::to_string(g) +
        std::to_string(b) +
        std::to_string(a) +
        ")";
}
