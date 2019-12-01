class ExplosionParams {
    constructor (radius, damage) {
        this.radius = radius;
        this.damage = damage;
    }
    
    static async readFrom (stream) {
        const radius = await stream.readDouble();
        const damage = await stream.readInt();
        return new ExplosionParams(radius, damage);
    }

    async writeTo (stream) {
        await stream.writeDouble(this.radius);
        await stream.writeInt(this.damage);
    }

    toString () {
        return 'ExplosionParams(' +
            this.radius + ',' +
            this.damage +
            ')';
    }
}

module.exports.ExplosionParams = ExplosionParams;
