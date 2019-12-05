class ColorFloat {
    constructor(r, g, b, a) {
        this.r = r;
        this.g = g;
        this.b = b;
        this.a = a;
    }
    static async readFrom(stream) {
        let r;
        r = await stream.readFloat();
        let g;
        g = await stream.readFloat();
        let b;
        b = await stream.readFloat();
        let a;
        a = await stream.readFloat();
        return new ColorFloat(r, g, b, a);
    }
    async writeTo(stream) {
        let r = this.r;
        await stream.writeFloat(r);
        let g = this.g;
        await stream.writeFloat(g);
        let b = this.b;
        await stream.writeFloat(b);
        let a = this.a;
        await stream.writeFloat(a);
    }
}
module.exports = { ColorFloat: ColorFloat }
