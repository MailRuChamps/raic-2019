import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct LootBox {
    Vec2Double position;
    Vec2Double size;
    Item item;
    this(Vec2Double position, Vec2Double size, Item item) {
        this.position = position;
        this.size = size;
        this.item = item;
    }
    static LootBox readFrom(Stream reader) {
        auto result = LootBox();
        result.position = Vec2Double.readFrom(reader);
        result.size = Vec2Double.readFrom(reader);
        result.item = Item.readFrom(reader);
        return result;
    }
    void writeTo(Stream writer) const {
        position.writeTo(writer);
        size.writeTo(writer);
        item.writeTo(writer);
    }
    string toString() const {
        return "LootBox" ~ "(" ~
            to!string(position) ~
            to!string(size) ~
            to!string(item) ~
            ")";
    }
}
