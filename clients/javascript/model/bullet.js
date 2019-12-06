const Vec2Double = require('./vec2-double').Vec2Double;
const ExplosionParams = require('./explosion-params').ExplosionParams;
class Bullet {
    constructor(weaponType, unitId, playerId, position, velocity, damage, size, explosionParams) {
        this.weaponType = weaponType;
        this.unitId = unitId;
        this.playerId = playerId;
        this.position = position;
        this.velocity = velocity;
        this.damage = damage;
        this.size = size;
        this.explosionParams = explosionParams;
    }
    static async readFrom(stream) {
        let weaponType;
        weaponType = await stream.readInt();
        let unitId;
        unitId = await stream.readInt();
        let playerId;
        playerId = await stream.readInt();
        let position;
        position = await Vec2Double.readFrom(stream);
        let velocity;
        velocity = await Vec2Double.readFrom(stream);
        let damage;
        damage = await stream.readInt();
        let size;
        size = await stream.readDouble();
        let explosionParams;
        if (await stream.readBool()) {
            explosionParams = await ExplosionParams.readFrom(stream);
        } else {
            explosionParams = null;
        }
        return new Bullet(weaponType, unitId, playerId, position, velocity, damage, size, explosionParams);
    }
    async writeTo(stream) {
        let weaponType = this.weaponType;
        await stream.writeInt(weaponType);
        let unitId = this.unitId;
        await stream.writeInt(unitId);
        let playerId = this.playerId;
        await stream.writeInt(playerId);
        let position = this.position;
        await position.writeTo(stream);
        let velocity = this.velocity;
        await velocity.writeTo(stream);
        let damage = this.damage;
        await stream.writeInt(damage);
        let size = this.size;
        await stream.writeDouble(size);
        let explosionParams = this.explosionParams;
        if (explosionParams === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await explosionParams.writeTo(stream);
        }
    }
}
module.exports = { Bullet: Bullet }
