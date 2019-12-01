const Vec2Float = require('./vec2float').Vec2Float;
const ColorFloat = require('./color-float').ColorFloat;

class ColoredVertex {
    constructor (position, color) {
        this.position = position;
        this.color = color;
    }
    
    static async readFrom (stream) {
        const position = await Vec2Float.readFrom(stream);
        const color = await ColorFloat.readFrom(stream);
        return new ColoredVertex(position, color);
    }
    
    async writeTo (stream) {
        await this.position.writeTo(stream);
        await this.color.writeTo(stream);
    }

    toString () {
        return 'ColoredVertex(' +
            this.position + ',' +
            this.color +
            ')';
    }
}

module.exports.ColoredVertex = ColoredVertex;
