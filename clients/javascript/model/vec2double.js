class Vec2Double {
    constructor (x, y) {
        this.x = x;
        this.y = y;
    }

    static async readFrom (stream) {
        const x = await stream.readDouble();
        const y = await stream.readDouble();
        return new Vec2Double(x, y);
    }

    async writeTo (stream) {
        await stream.writeDouble(this.x);
        await stream.writeDouble(this.y);
    }
    
    toString () {
        return 'Vec2Double(' +
            this.x + ',' +
            this.y +
            ')';
    }
}

module.exports.Vec2Double = Vec2Double;
