class Vec2Double {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
    static async readFrom(stream) {
        let x;
        x = await stream.readDouble();
        let y;
        y = await stream.readDouble();
        return new Vec2Double(x, y);
    }
    async writeTo(stream) {
        let x = this.x;
        await stream.writeDouble(x);
        let y = this.y;
        await stream.writeDouble(y);
    }
}
module.exports = { Vec2Double: Vec2Double }
