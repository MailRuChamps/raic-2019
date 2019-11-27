#ifndef _MODEL_WEAPON_PARAMS_HPP_
#define _MODEL_WEAPON_PARAMS_HPP_

#include "../Stream.hpp"
#include <string>
#include <stdexcept>
#include "BulletParams.hpp"
#include <memory>
#include <stdexcept>
#include "ExplosionParams.hpp"

class WeaponParams {
public:
    int magazineSize;
    double fireRate;
    double reloadTime;
    double minSpread;
    double maxSpread;
    double recoil;
    double aimSpeed;
    BulletParams bullet;
    std::shared_ptr<ExplosionParams> explosion;
    WeaponParams();
    WeaponParams(int magazineSize, double fireRate, double reloadTime, double minSpread, double maxSpread, double recoil, double aimSpeed, BulletParams bullet, std::shared_ptr<ExplosionParams> explosion);
    static WeaponParams readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
