const WeaponType = require('./weapon-type').WeaponType;
const WeaponParams = require('./weapon-params').WeaponParams;

class Weapon {
    constructor (typ, params, magazine, wasShooting, spread, fireTimer, lastAngle, lastFireTick) {
        this.typ = typ;
        this.params = params;
        this.magazine = magazine;
        this.wasShooting = wasShooting;
        this.spread = spread;
        this.fireTimer = fireTimer;
        this.lastAngle = lastAngle;
        this.lastFireTick = lastFireTick;
    }
    
    static async readFrom (stream) {
        const typ = await WeaponType.readFrom(stream);
        const params = await WeaponParams.readFrom(stream);
        const magazine = await stream.readInt();
        const wasShooting = await stream.readBool();
        const spread = await stream.readDouble();
        let fireTimer;
        if (await stream.readBool()) {
            fireTimer = await stream.readDouble();
        } else {
            fireTimer = null;
        }
        let lastAngle;
        if (await stream.readBool()) {
            lastAngle = await stream.readDouble();
        } else {
            lastAngle = null;
        }
        let lastFireTick;
        if (await stream.readBool()) {
            lastFireTick = await stream.readInt();
        } else {
            lastFireTick = null;
        }
        return new Weapon(typ, params, magazine, wasShooting, spread, fireTimer, lastAngle, lastFireTick)
    }
    
    async writeTo (stream) {
        await stream.writeInt(this.typ.discriminant);
        await this.params.writeTo(stream);
        await stream.writeInt(this.magazine);
        await stream.writeBool(this.wasShooting);
        await stream.writeDouble(this.spread);
        if (this.fireTimer === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeDouble(this.fireTimer);
        }
        if (this.lastAngle === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeDouble(this.lastAngle);
        }
        if (this.lastFireTick === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeInt(this.lastFireTick);
        }
    }

    toString () {
        return 'Weapon(' +
            this.typ + ',' +
            this.params + ',' +
            this.magazine + ',' +
            this.wasShooting + ',' +
            this.spread + ',' +
            this.fireTimer + ',' +
            this.lastAngle + ',' +
            this.lastFireTick +
            ')';
    }
}

module.exports.Weapon = Weapon;
