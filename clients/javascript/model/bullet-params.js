class BulletParams {
    constructor (speed, size, damage) {
        this.speed = speed;
        this.size = size;
        this.damage = damage;
    }
    
    static async readFrom (stream) {
        const speed = await stream.readDouble();
        const size = await stream.readDouble();
        const damage = await stream.readInt();
        return new BulletParams(speed, size, damage);
    }

    async writeTo (stream) {
        await stream.writeDouble(this.speed);
        await stream.writeDouble(this.size);
        await stream.writeInt(this.damage);
    }

    toString () {
        return 'BulletParams(' +
            this.speed + ',' +
            this.size + ',' +
            this.damage +
            ')';
    }
}

module.exports.BulletParams = BulletParams;
