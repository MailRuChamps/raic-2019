#include "Bullet.hpp"

Bullet::Bullet() { }
Bullet::Bullet(WeaponType weaponType, int unitId, int playerId, Vec2Double position, Vec2Double velocity, int damage, double size, std::shared_ptr<ExplosionParams> explosionParams) : weaponType(weaponType), unitId(unitId), playerId(playerId), position(position), velocity(velocity), damage(damage), size(size), explosionParams(explosionParams) { }
Bullet Bullet::readFrom(InputStream& stream) {
    Bullet result;
    switch (stream.readInt()) {
    case 0:
        result.weaponType = WeaponType::PISTOL;
        break;
    case 1:
        result.weaponType = WeaponType::ASSAULT_RIFLE;
        break;
    case 2:
        result.weaponType = WeaponType::ROCKET_LAUNCHER;
        break;
    default:
        throw std::runtime_error("Unexpected discriminant value");
    }
    result.unitId = stream.readInt();
    result.playerId = stream.readInt();
    result.position = Vec2Double::readFrom(stream);
    result.velocity = Vec2Double::readFrom(stream);
    result.damage = stream.readInt();
    result.size = stream.readDouble();
    if (stream.readBool()) {
        result.explosionParams = std::shared_ptr<ExplosionParams>(new ExplosionParams());
        *result.explosionParams = ExplosionParams::readFrom(stream);
    } else {
        result.explosionParams = std::shared_ptr<ExplosionParams>();
    }
    return result;
}
void Bullet::writeTo(OutputStream& stream) const {
    stream.write((int)(weaponType));
    stream.write(unitId);
    stream.write(playerId);
    position.writeTo(stream);
    velocity.writeTo(stream);
    stream.write(damage);
    stream.write(size);
    if (explosionParams) {
        stream.write(false);
    } else {
        stream.write(true);
        (*explosionParams).writeTo(stream);
    }
}
std::string Bullet::toString() const {
    return std::string("Bullet") + "(" +
        "TODO" + 
        std::to_string(unitId) +
        std::to_string(playerId) +
        position.toString() +
        velocity.toString() +
        std::to_string(damage) +
        std::to_string(size) +
        "TODO" + 
        ")";
}
