#ifndef _MODEL_PROPERTIES_HPP_
#define _MODEL_PROPERTIES_HPP_

#include "../Stream.hpp"
#include <string>
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <unordered_map>
#include <stdexcept>
#include "WeaponType.hpp"
#include <stdexcept>
#include "WeaponParams.hpp"
#include <stdexcept>
#include "BulletParams.hpp"
#include <memory>
#include <stdexcept>
#include "ExplosionParams.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "ExplosionParams.hpp"

class Properties {
public:
    int maxTickCount;
    int teamSize;
    double ticksPerSecond;
    int updatesPerTick;
    Vec2Double lootBoxSize;
    Vec2Double unitSize;
    double unitMaxHorizontalSpeed;
    double unitFallSpeed;
    double unitJumpTime;
    double unitJumpSpeed;
    double jumpPadJumpTime;
    double jumpPadJumpSpeed;
    int unitMaxHealth;
    int healthPackHealth;
    std::unordered_map<WeaponType, WeaponParams> weaponParams;
    Vec2Double mineSize;
    ExplosionParams mineExplosionParams;
    double minePrepareTime;
    double mineTriggerTime;
    double mineTriggerRadius;
    int killScore;
    Properties();
    Properties(int maxTickCount, int teamSize, double ticksPerSecond, int updatesPerTick, Vec2Double lootBoxSize, Vec2Double unitSize, double unitMaxHorizontalSpeed, double unitFallSpeed, double unitJumpTime, double unitJumpSpeed, double jumpPadJumpTime, double jumpPadJumpSpeed, int unitMaxHealth, int healthPackHealth, std::unordered_map<WeaponType, WeaponParams> weaponParams, Vec2Double mineSize, ExplosionParams mineExplosionParams, double minePrepareTime, double mineTriggerTime, double mineTriggerRadius, int killScore);
    static Properties readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
