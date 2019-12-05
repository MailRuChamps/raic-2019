class BulletParams {
    constructor(speed, size, damage) {
        this.speed = speed;
        this.size = size;
        this.damage = damage;
    }
    static async readFrom(stream) {
        let speed;
        speed = await stream.readDouble();
        let size;
        size = await stream.readDouble();
        let damage;
        damage = await stream.readInt();
        return new BulletParams(speed, size, damage);
    }
    async writeTo(stream) {
        let speed = this.speed;
        await stream.writeDouble(speed);
        let size = this.size;
        await stream.writeDouble(size);
        let damage = this.damage;
        await stream.writeInt(damage);
    }
}
module.exports = { BulletParams: BulletParams }
