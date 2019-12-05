const BulletParams = require('./bullet-params').BulletParams;
const ExplosionParams = require('./explosion-params').ExplosionParams;
class WeaponParams {
    constructor(magazineSize, fireRate, reloadTime, minSpread, maxSpread, recoil, aimSpeed, bullet, explosion) {
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
    static async readFrom(stream) {
        let magazineSize;
        magazineSize = await stream.readInt();
        let fireRate;
        fireRate = await stream.readDouble();
        let reloadTime;
        reloadTime = await stream.readDouble();
        let minSpread;
        minSpread = await stream.readDouble();
        let maxSpread;
        maxSpread = await stream.readDouble();
        let recoil;
        recoil = await stream.readDouble();
        let aimSpeed;
        aimSpeed = await stream.readDouble();
        let bullet;
        bullet = await BulletParams.readFrom(stream);
        let explosion;
        if (await stream.readBool()) {
            explosion = await ExplosionParams.readFrom(stream);
        } else {
            explosion = null;
        }
        return new WeaponParams(magazineSize, fireRate, reloadTime, minSpread, maxSpread, recoil, aimSpeed, bullet, explosion);
    }
    async writeTo(stream) {
        let magazineSize = this.magazineSize;
        await stream.writeInt(magazineSize);
        let fireRate = this.fireRate;
        await stream.writeDouble(fireRate);
        let reloadTime = this.reloadTime;
        await stream.writeDouble(reloadTime);
        let minSpread = this.minSpread;
        await stream.writeDouble(minSpread);
        let maxSpread = this.maxSpread;
        await stream.writeDouble(maxSpread);
        let recoil = this.recoil;
        await stream.writeDouble(recoil);
        let aimSpeed = this.aimSpeed;
        await stream.writeDouble(aimSpeed);
        let bullet = this.bullet;
        await bullet.writeTo(stream);
        let explosion = this.explosion;
        if (explosion === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await explosion.writeTo(stream);
        }
    }
}
module.exports = { WeaponParams: WeaponParams }
