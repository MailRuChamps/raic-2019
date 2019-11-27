#include "LootBox.hpp"

LootBox::LootBox() { }
LootBox::LootBox(Vec2Double position, Vec2Double size, std::shared_ptr<Item> item) : position(position), size(size), item(item) { }
LootBox LootBox::readFrom(InputStream& stream) {
    LootBox result;
    result.position = Vec2Double::readFrom(stream);
    result.size = Vec2Double::readFrom(stream);
    result.item = Item::readFrom(stream);
    return result;
}
void LootBox::writeTo(OutputStream& stream) const {
    position.writeTo(stream);
    size.writeTo(stream);
    item->writeTo(stream);
}
std::string LootBox::toString() const {
    return std::string("LootBox") + "(" +
        position.toString() +
        size.toString() +
        item->toString() +
        ")";
}
