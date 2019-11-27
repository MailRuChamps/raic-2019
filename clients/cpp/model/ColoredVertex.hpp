#ifndef _MODEL_COLORED_VERTEX_HPP_
#define _MODEL_COLORED_VERTEX_HPP_

#include "../Stream.hpp"
#include <string>
#include <stdexcept>
#include "Vec2Float.hpp"
#include <stdexcept>
#include "ColorFloat.hpp"

class ColoredVertex {
public:
    Vec2Float position;
    ColorFloat color;
    ColoredVertex();
    ColoredVertex(Vec2Float position, ColorFloat color);
    static ColoredVertex readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
