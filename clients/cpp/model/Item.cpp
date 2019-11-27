#include "Item.hpp"


Item::HealthPack::HealthPack() { }
Item::HealthPack::HealthPack(int health) : health(health) { }
Item::HealthPack Item::HealthPack::readFrom(InputStream& stream) {
    Item::HealthPack result;
    result.health = stream.readInt();
    return result;
}
void Item::HealthPack::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write(health);
}
std::string Item::HealthPack::toString() const {
    return std::string("Item::HealthPack") + "(" +
        std::to_string(health) +
        ")";
}

Item::Weapon::Weapon() { }
Item::Weapon::Weapon(WeaponType weaponType) : weaponType(weaponType) { }
Item::Weapon Item::Weapon::readFrom(InputStream& stream) {
    Item::Weapon result;
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
    return result;
}
void Item::Weapon::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write((int)(weaponType));
}
std::string Item::Weapon::toString() const {
    return std::string("Item::Weapon") + "(" +
        "TODO" + 
        ")";
}

Item::Mine::Mine() { }
Item::Mine Item::Mine::readFrom(InputStream& stream) {
    Item::Mine result;
    return result;
}
void Item::Mine::writeTo(OutputStream& stream) const {
    stream.write(TAG);
}
std::string Item::Mine::toString() const {
    return std::string("Item::Mine") + "(" +
        ")";
}
std::shared_ptr<Item> Item::readFrom(InputStream& stream) {
    switch (stream.readInt()) {
    case 0:
        return std::shared_ptr<Item::HealthPack>(new Item::HealthPack(Item::HealthPack::readFrom(stream)));
    case 1:
        return std::shared_ptr<Item::Weapon>(new Item::Weapon(Item::Weapon::readFrom(stream)));
    case 2:
        return std::shared_ptr<Item::Mine>(new Item::Mine(Item::Mine::readFrom(stream)));
    default:
        throw std::runtime_error("Unexpected discriminant value");
    }
};
