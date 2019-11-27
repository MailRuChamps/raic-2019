#ifndef _MODEL_LOOT_BOX_HPP_
#define _MODEL_LOOT_BOX_HPP_

#include "../Stream.hpp"
#include <string>
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Item.hpp"
#include <stdexcept>
#include "WeaponType.hpp"

class LootBox {
public:
    Vec2Double position;
    Vec2Double size;
    std::shared_ptr<Item> item;
    LootBox();
    LootBox(Vec2Double position, Vec2Double size, std::shared_ptr<Item> item);
    static LootBox readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
