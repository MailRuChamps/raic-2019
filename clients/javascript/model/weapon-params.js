const BulletParams = require('./bullet-params').BulletParams;
const ExplosionParams = require('./explosion-params').ExplosionParams;

class WeaponParams {
    constructor (magazineSize, fireRate, reloadTime, minSpread, maxSpread, recoil, aimSpeed, bullet, explosion) {
        this.magazineSize = magazineSize;
        this.fireRate = fireRate;
        this.reloadTime = reloadTime;
        this.minSpread = minSpread;
        this.maxSpread = maxSpread;
        this.recoil = recoil;
        this.aimSpeed = aimSpeed;
        this.bullet = bullet;
        this.explosion = explosion;
    }

    static async readFrom (stream) {
        const magazineSize = await stream.readInt();
        const fireRate = await stream.readDouble();
        const reloadTime = await stream.readDouble();
        const minSpread = await stream.readDouble();
        const maxSpread = await stream.readDouble();
        const recoil = await stream.readDouble();
        const aimSpeed = await stream.readDouble();
        const bullet = await BulletParams.readFrom(stream);
        let explosion;
        if (await stream.readBool()) {
            explosion = await ExplosionParams.readFrom(stream);
        } else {
            explosion = null;
        }

        return new WeaponParams(magazineSize, fireRate, reloadTime, minSpread, maxSpread, recoil, aimSpeed, bullet, explosion);
    }

    async writeTo (stream) {
        await stream.writeInt(this.magazineSize);
        await stream.writeDouble(this.fireRate);
        await stream.writeDouble(this.reloadTime);
        await stream.writeDouble(this.minSpread);
        await stream.writeDouble(this.maxSpread);
        await stream.writeDouble(this.recoil);
        await stream.writeDouble(this.aimSpeed);
        await this.bullet.writeTo(stream);
        if (this.explosion === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await this.explosion.writeTo(stream);
        }
    }
    
    toString () {
        return 'WeaponParams(' +
            this.magazineSize + ',' +
            this.fireRate + ',' +
            this.reloadTime + ',' +
            this.minSpread + ',' +
            this.maxSpread + ',' +
            this.recoil + ',' +
            this.aimSpeed + ',' +
            this.bullet + ',' +
            this.explosion +
            ')';
    }
}

module.exports.WeaponParams = WeaponParams;
