class ColorFloat {
    constructor (r, g, b, a) {
        this.r = r;
        this.g = g;
        this.b = b;
        this.a = a;
    }
    
    static async readFrom (stream) {
        const r = await stream.readFloat();
        const g = await stream.readFloat();
        const b = await stream.readFloat();
        const a = await stream.readFloat();
        return new ColorFloat(r, g, b, a);
    }
    
    async writeTo (stream) {
        await stream.writeFloat(this.r);
        await stream.writeFloat(this.g);
        await stream.writeFloat(this.b);
        await stream.writeFloat(this.a);
    }

    toString () {
        return 'ColorFloat(' +
            this.r + ',' +
            this.g + ',' +
            this.b + ',' +
            this.a +
            ')';
    }
}

module.exports.ColorFloat = ColorFloat;
