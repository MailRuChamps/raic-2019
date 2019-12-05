const Vec2Double = require('./vec2-double').Vec2Double;
const Item = require('./item').Item;
class LootBox {
    constructor(position, size, item) {
        this.position = position;
        this.size = size;
        this.item = item;
    }
    static async readFrom(stream) {
        let position;
        position = await Vec2Double.readFrom(stream);
        let size;
        size = await Vec2Double.readFrom(stream);
        let item;
        item = await Item.readFrom(stream);
        return new LootBox(position, size, item);
    }
    async writeTo(stream) {
        let position = this.position;
        await position.writeTo(stream);
        let size = this.size;
        await size.writeTo(stream);
        let item = this.item;
        await item.writeTo(stream);
    }
}
module.exports = { LootBox: LootBox }
