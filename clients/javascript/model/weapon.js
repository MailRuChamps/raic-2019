const WeaponParams = require('./weapon-params').WeaponParams;
class Weapon {
    constructor(typ, params, magazine, wasShooting, spread, fireTimer, lastAngle, lastFireTick) {
        this.typ = typ;
        this.params = params;
        this.magazine = magazine;
        this.wasShooting = wasShooting;
        this.spread = spread;
        this.fireTimer = fireTimer;
        this.lastAngle = lastAngle;
        this.lastFireTick = lastFireTick;
    }
    static async readFrom(stream) {
        let typ;
        typ = stream.readInt();
        let params;
        params = await WeaponParams.readFrom(stream);
        let magazine;
        magazine = await stream.readInt();
        let wasShooting;
        wasShooting = await stream.readBool();
        let spread;
        spread = await stream.readDouble();
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
        return new Weapon(typ, params, magazine, wasShooting, spread, fireTimer, lastAngle, lastFireTick);
    }
    async writeTo(stream) {
        let typ = this.typ;
        await stream.writeInt(typ);
        let params = this.params;
        await params.writeTo(stream);
        let magazine = this.magazine;
        await stream.writeInt(magazine);
        let wasShooting = this.wasShooting;
        await stream.writeBool(wasShooting);
        let spread = this.spread;
        await stream.writeDouble(spread);
        let fireTimer = this.fireTimer;
        if (fireTimer === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeDouble(fireTimer);
        }
        let lastAngle = this.lastAngle;
        if (lastAngle === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeDouble(lastAngle);
        }
        let lastFireTick = this.lastFireTick;
        if (lastFireTick === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeInt(lastFireTick);
        }
    }
}
module.exports = { Weapon: Weapon }
