#ifndef _MODEL_UNIT_HPP_
#define _MODEL_UNIT_HPP_

#include "../Stream.hpp"
#include <string>
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "JumpState.hpp"
#include <memory>
#include <stdexcept>
#include "Weapon.hpp"
#include <stdexcept>
#include "WeaponType.hpp"
#include <stdexcept>
#include "WeaponParams.hpp"
#include <stdexcept>
#include "BulletParams.hpp"
#include <memory>
#include <stdexcept>
#include "ExplosionParams.hpp"
#include <memory>
#include <memory>
#include <memory>

class Unit {
public:
    int playerId;
    int id;
    int health;
    Vec2Double position;
    Vec2Double size;
    JumpState jumpState;
    bool walkedRight;
    bool stand;
    bool onGround;
    bool onLadder;
    int mines;
    std::shared_ptr<Weapon> weapon;
    Unit();
    Unit(int playerId, int id, int health, Vec2Double position, Vec2Double size, JumpState jumpState, bool walkedRight, bool stand, bool onGround, bool onLadder, int mines, std::shared_ptr<Weapon> weapon);
    static Unit readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
