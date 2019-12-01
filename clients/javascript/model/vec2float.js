class Vec2Float {
    constructor ( x, y) {
        this.x = x;
        this.y = y;
    }
    
    static async readFrom (stream) {
        const x = await stream.readFloat();
        const y = await stream.readFloat();
        return new Vec2Float(x, y);
    }

    async writeTo (stream) {
        await stream.writeFloat(this.x);
        await stream.writeFloat(this.y);
    }
    
    toString () {
        return 'Vec2Float(' +
            this.x + ',' +
            this.y +
            ')';
    }
}

module.exports.Vec2Float = Vec2Float;
