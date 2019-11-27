#include "Weapon.hpp"

Weapon::Weapon() { }
Weapon::Weapon(WeaponType typ, WeaponParams params, int magazine, bool wasShooting, double spread, std::shared_ptr<double> fireTimer, std::shared_ptr<double> lastAngle, std::shared_ptr<int> lastFireTick) : typ(typ), params(params), magazine(magazine), wasShooting(wasShooting), spread(spread), fireTimer(fireTimer), lastAngle(lastAngle), lastFireTick(lastFireTick) { }
Weapon Weapon::readFrom(InputStream& stream) {
    Weapon result;
    switch (stream.readInt()) {
    case 0:
        result.typ = WeaponType::PISTOL;
        break;
    case 1:
        result.typ = WeaponType::ASSAULT_RIFLE;
        break;
    case 2:
        result.typ = WeaponType::ROCKET_LAUNCHER;
        break;
    default:
        throw std::runtime_error("Unexpected discriminant value");
    }
    result.params = WeaponParams::readFrom(stream);
    result.magazine = stream.readInt();
    result.wasShooting = stream.readBool();
    result.spread = stream.readDouble();
    if (stream.readBool()) {
        result.fireTimer = std::shared_ptr<double>(new double());
        *result.fireTimer = stream.readDouble();
    } else {
        result.fireTimer = std::shared_ptr<double>();
    }
    if (stream.readBool()) {
        result.lastAngle = std::shared_ptr<double>(new double());
        *result.lastAngle = stream.readDouble();
    } else {
        result.lastAngle = std::shared_ptr<double>();
    }
    if (stream.readBool()) {
        result.lastFireTick = std::shared_ptr<int>(new int());
        *result.lastFireTick = stream.readInt();
    } else {
        result.lastFireTick = std::shared_ptr<int>();
    }
    return result;
}
void Weapon::writeTo(OutputStream& stream) const {
    stream.write((int)(typ));
    params.writeTo(stream);
    stream.write(magazine);
    stream.write(wasShooting);
    stream.write(spread);
    if (fireTimer) {
        stream.write(false);
    } else {
        stream.write(true);
        stream.write((*fireTimer));
    }
    if (lastAngle) {
        stream.write(false);
    } else {
        stream.write(true);
        stream.write((*lastAngle));
    }
    if (lastFireTick) {
        stream.write(false);
    } else {
        stream.write(true);
        stream.write((*lastFireTick));
    }
}
std::string Weapon::toString() const {
    return std::string("Weapon") + "(" +
        "TODO" + 
        params.toString() +
        std::to_string(magazine) +
        (wasShooting ? "true" : "false") + 
        std::to_string(spread) +
        "TODO" + 
        "TODO" + 
        "TODO" + 
        ")";
}
