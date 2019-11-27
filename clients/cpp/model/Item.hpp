#ifndef _MODEL_ITEM_HPP_
#define _MODEL_ITEM_HPP_

#include "../Stream.hpp"
#include <memory>
#include <string>
#include <stdexcept>
#include <stdexcept>
#include "WeaponType.hpp"

class Item {
public:
    class HealthPack;
    class Weapon;
    class Mine;

    static std::shared_ptr<Item> readFrom(InputStream& stream);
    virtual void writeTo(OutputStream& stream) const = 0;
    virtual std::string toString() const = 0;
};

class Item::HealthPack : public Item {
public:
    static const int TAG = 0;
public:
    int health;
    HealthPack();
    HealthPack(int health);
    static HealthPack readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const override;
};

class Item::Weapon : public Item {
public:
    static const int TAG = 1;
public:
    WeaponType weaponType;
    Weapon();
    Weapon(WeaponType weaponType);
    static Weapon readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const override;
};

class Item::Mine : public Item {
public:
    static const int TAG = 2;
public:
    Mine();
    static Mine readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const override;
};

#endif
