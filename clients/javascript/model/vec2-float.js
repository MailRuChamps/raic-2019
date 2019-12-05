class Vec2Float {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
    static async readFrom(stream) {
        let x;
        x = await stream.readFloat();
        let y;
        y = await stream.readFloat();
        return new Vec2Float(x, y);
    }
    async writeTo(stream) {
        let x = this.x;
        await stream.writeFloat(x);
        let y = this.y;
        await stream.writeFloat(y);
    }
}
module.exports = { Vec2Float: Vec2Float }
