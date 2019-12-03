#ifndef _MODEL_UNIT_ACTION_HPP_
#define _MODEL_UNIT_ACTION_HPP_

#include "../Stream.hpp"
#include <string>
#include <stdexcept>
#include "Vec2Double.hpp"

class UnitAction {
public:
    double velocity;
    bool jump;
    bool jumpDown;
    Vec2Double aim;
    bool shoot;
    bool reload;
    bool swapWeapon;
    bool plantMine;
    UnitAction();
    UnitAction(double velocity, bool jump, bool jumpDown, Vec2Double aim, bool shoot, bool reload, bool swapWeapon, bool plantMine);
    static UnitAction readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
