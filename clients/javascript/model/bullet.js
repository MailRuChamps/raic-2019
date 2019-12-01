const WeaponType = require('./weapon-type').WeaponType;
const Vec2Double = require('./vec2double').Vec2Double;
const ExplosionParams = require('./explosion-params').ExplosionParams;

class Bullet {
    constructor (weaponType, unitId, playerId, position, velocity, damage, size, explosionParams) {
        this.weaponType = weaponType;
        this.unitId = unitId;
        this.playerId = playerId;
        this.position = position;
        this.velocity = velocity;
        this.damage = damage;
        this.size = size;
        this.explosionParams = explosionParams;
    }

    static async readFrom (stream) {
        const weaponType = await WeaponType.readFrom(stream);
        const unitId = await stream.readInt();
        const playerId = await stream.readInt();
        const position = await Vec2Double.readFrom(stream);
        const velocity = await Vec2Double.readFrom(stream);
        const damage = await stream.readInt();
        const size = await stream.readDouble();
        let explosionParams;
        if (await stream.readBool()) {
            explosionParams = await ExplosionParams.readFrom(stream);
        } else {
            explosionParams = null;
        }
        return new Bullet(weaponType, unitId, playerId, position, velocity, damage, size, explosionParams);
    }

    async writeTo (stream) {
        await stream.writeInt(this.weaponType.discriminant);
        await stream.writeInt(this.unitId);
        await stream.writeInt(this.playerId);
        await this.position.writeTo(stream);
        await this.velocity.writeTo(stream);
        await stream.writeInt(this.damage);
        await stream.writeDouble(this.size);
        if (this.explosionParams === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await this.explosionParams.writeTo(stream);
        }
    }

    toString () {
        return 'Bullet(' +
            this.weaponType + ',' +
            this.unitId + ',' +
            this.playerId + ',' +
            this.position + ',' +
            this.velocity + ',' +
            this.damage + ',' +
            this.size + ',' +
            this.explosionParams +
            ')';
    }
}

module.exports.Bullet = Bullet;