#ifndef _MODEL_WEAPON_HPP_
#define _MODEL_WEAPON_HPP_

#include "../Stream.hpp"
#include <string>
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

class Weapon {
public:
    WeaponType typ;
    WeaponParams params;
    int magazine;
    bool wasShooting;
    double spread;
    std::shared_ptr<double> fireTimer;
    std::shared_ptr<double> lastAngle;
    std::shared_ptr<int> lastFireTick;
    Weapon();
    Weapon(WeaponType typ, WeaponParams params, int magazine, bool wasShooting, double spread, std::shared_ptr<double> fireTimer, std::shared_ptr<double> lastAngle, std::shared_ptr<int> lastFireTick);
    static Weapon readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
