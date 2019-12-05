const Vec2Float = require('./vec2-float').Vec2Float;
const ColorFloat = require('./color-float').ColorFloat;
class ColoredVertex {
    constructor(position, color) {
        this.position = position;
        this.color = color;
    }
    static async readFrom(stream) {
        let position;
        position = await Vec2Float.readFrom(stream);
        let color;
        color = await ColorFloat.readFrom(stream);
        return new ColoredVertex(position, color);
    }
    async writeTo(stream) {
        let position = this.position;
        await position.writeTo(stream);
        let color = this.color;
        await color.writeTo(stream);
    }
}
module.exports = { ColoredVertex: ColoredVertex }
