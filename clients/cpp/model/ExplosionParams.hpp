#ifndef _MODEL_EXPLOSION_PARAMS_HPP_
#define _MODEL_EXPLOSION_PARAMS_HPP_

#include "../Stream.hpp"
#include <string>

class ExplosionParams {
public:
    double radius;
    int damage;
    ExplosionParams();
    ExplosionParams(double radius, int damage);
    static ExplosionParams readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
