const Vec2Double = require('./vec2double').Vec2Double;
const Item = require('./item').Item;

class LootBox {
    constructor (position, size, item) {
        this.position = position;
        this.size = size;
        this.item = item;
    }
    
    static async readFrom (stream) {
        const position = await Vec2Double.readFrom(stream);
        const size = await Vec2Double.readFrom(stream);
        const item = await Item.readFrom(stream);
        return new LootBox(position, size, item);
    }
    
    async writeTo (stream) {
        await this.position.writeTo(stream);
        await this.size.writeTo(stream);
        await this.item.writeTo(stream);
    }

    toString () {
        return 'LootBox(' +
            this.position + ',' +
            this.size + ',' +
            this.item +
            ')';
    }
}

module.exports.LootBox = LootBox;
