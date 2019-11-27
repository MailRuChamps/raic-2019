#include "WeaponParams.hpp"

WeaponParams::WeaponParams() { }
WeaponParams::WeaponParams(int magazineSize, double fireRate, double reloadTime, double minSpread, double maxSpread, double recoil, double aimSpeed, BulletParams bullet, std::shared_ptr<ExplosionParams> explosion) : magazineSize(magazineSize), fireRate(fireRate), reloadTime(reloadTime), minSpread(minSpread), maxSpread(maxSpread), recoil(recoil), aimSpeed(aimSpeed), bullet(bullet), explosion(explosion) { }
WeaponParams WeaponParams::readFrom(InputStream& stream) {
    WeaponParams result;
    result.magazineSize = stream.readInt();
    result.fireRate = stream.readDouble();
    result.reloadTime = stream.readDouble();
    result.minSpread = stream.readDouble();
    result.maxSpread = stream.readDouble();
    result.recoil = stream.readDouble();
    result.aimSpeed = stream.readDouble();
    result.bullet = BulletParams::readFrom(stream);
    if (stream.readBool()) {
        result.explosion = std::shared_ptr<ExplosionParams>(new ExplosionParams());
        *result.explosion = ExplosionParams::readFrom(stream);
    } else {
        result.explosion = std::shared_ptr<ExplosionParams>();
    }
    return result;
}
void WeaponParams::writeTo(OutputStream& stream) const {
    stream.write(magazineSize);
    stream.write(fireRate);
    stream.write(reloadTime);
    stream.write(minSpread);
    stream.write(maxSpread);
    stream.write(recoil);
    stream.write(aimSpeed);
    bullet.writeTo(stream);
    if (explosion) {
        stream.write(false);
    } else {
        stream.write(true);
        (*explosion).writeTo(stream);
    }
}
std::string WeaponParams::toString() const {
    return std::string("WeaponParams") + "(" +
        std::to_string(magazineSize) +
        std::to_string(fireRate) +
        std::to_string(reloadTime) +
        std::to_string(minSpread) +
        std::to_string(maxSpread) +
        std::to_string(recoil) +
        std::to_string(aimSpeed) +
        bullet.toString() +
        "TODO" + 
        ")";
}
