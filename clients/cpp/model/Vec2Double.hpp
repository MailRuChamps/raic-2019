#ifndef _MODEL_VEC2_DOUBLE_HPP_
#define _MODEL_VEC2_DOUBLE_HPP_

#include "../Stream.hpp"
#include <string>

class Vec2Double {
public:
    double x;
    double y;
    Vec2Double();
    Vec2Double(double x, double y);
    static Vec2Double readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
