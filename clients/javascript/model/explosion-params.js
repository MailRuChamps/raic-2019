class ExplosionParams {
    constructor(radius, damage) {
        this.radius = radius;
        this.damage = damage;
    }
    static async readFrom(stream) {
        let radius;
        radius = await stream.readDouble();
        let damage;
        damage = await stream.readInt();
        return new ExplosionParams(radius, damage);
    }
    async writeTo(stream) {
        let radius = this.radius;
        await stream.writeDouble(radius);
        let damage = this.damage;
        await stream.writeInt(damage);
    }
}
module.exports = { ExplosionParams: ExplosionParams }
