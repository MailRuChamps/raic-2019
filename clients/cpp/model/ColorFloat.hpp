#ifndef _MODEL_COLOR_FLOAT_HPP_
#define _MODEL_COLOR_FLOAT_HPP_

#include "../Stream.hpp"
#include <string>

class ColorFloat {
public:
    float r;
    float g;
    float b;
    float a;
    ColorFloat();
    ColorFloat(float r, float g, float b, float a);
    static ColorFloat readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
