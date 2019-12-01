const Vec2Double = require('./vec2double').Vec2Double;
const WeaponType = require('./weapon-type').WeaponType;
const WeaponParams = require('./weapon-params').WeaponParams;
const ExplosionParams = require('./explosion-params').ExplosionParams;

class Properties {
    constructor (maxTickCount, teamSize, ticksPerSecond, updatesPerTick, lootBoxSize, unitSize, unitMaxHorizontalSpeed, unitFallSpeed, unitJumpTime, unitJumpSpeed, jumpPadJumpTime, jumpPadJumpSpeed, unitMaxHealth, healthPackHealth, weaponParams, mineSize, mineExplosionParams, minePrepareTime, mineTriggerTime, mineTriggerRadius, killScore) {
        this.maxTickCount = maxTickCount;
        this.teamSize = teamSize;
        this.ticksPerSecond = ticksPerSecond;
        this.updatesPerTick = updatesPerTick;
        this.lootBoxSize = lootBoxSize;
        this.unitSize = unitSize;
        this.unitMaxHorizontalSpeed = unitMaxHorizontalSpeed;
        this.unitFallSpeed = unitFallSpeed;
        this.unitJumpTime = unitJumpTime;
        this.unitJumpSpeed = unitJumpSpeed;
        this.jumpPadJumpTime = jumpPadJumpTime;
        this.jumpPadJumpSpeed = jumpPadJumpSpeed;
        this.unitMaxHealth = unitMaxHealth;
        this.healthPackHealth = healthPackHealth;
        this.weaponParams = weaponParams;
        this.mineSize = mineSize;
        this.mineExplosionParams = mineExplosionParams;
        this.minePrepareTime = minePrepareTime;
        this.mineTriggerTime = mineTriggerTime;
        this.mineTriggerRadius = mineTriggerRadius;
        this.killScore = killScore;
    }

    static async readFrom (stream) {
        const maxTickCount = await stream.readInt();
        const teamSize = await stream.readInt();
        const ticksPerSecond = await stream.readDouble();
        const updatesPerTick = await stream.readInt();
        const lootBoxSize = await Vec2Double.readFrom(stream);
        const unitSize = await Vec2Double.readFrom(stream);
        const unitMaxHorizontalSpeed = await stream.readDouble();
        const unitFallSpeed = await stream.readDouble();
        const unitJumpTime = await stream.readDouble();
        const unitJumpSpeed = await stream.readDouble();
        const jumpPadJumpTime = await stream.readDouble();
        const jumpPadJumpSpeed = await stream.readDouble();
        const unitMaxHealth = await stream.readInt();
        const healthPackHealth = await stream.readInt();
        const weaponParams = {};
        for (let i = 0, paramsSize = await stream.readInt(); i < paramsSize; i++) {
            const weaponType = await WeaponType.readFrom(stream);
            weaponParams[weaponType.discriminant] = await WeaponParams.readFrom(stream);
        }
        const mineSize = await Vec2Double.readFrom(stream);
        const mineExplosionParams = await ExplosionParams.readFrom(stream);
        const minePrepareTime = await stream.readDouble();
        const mineTriggerTime = await stream.readDouble();
        const mineTriggerRadius = await stream.readDouble();
        const killScore = await stream.readInt();
        return new Properties(maxTickCount, teamSize, ticksPerSecond, updatesPerTick, lootBoxSize, unitSize, unitMaxHorizontalSpeed, unitFallSpeed, unitJumpTime, unitJumpSpeed, jumpPadJumpTime, jumpPadJumpSpeed, unitMaxHealth, healthPackHealth, weaponParams, mineSize, mineExplosionParams, minePrepareTime, mineTriggerTime, mineTriggerRadius, killScore);
    }

    async writeTo (stream) {
        await stream.writeInt(this.maxTickCount);
        await stream.writeInt(this.teamSize);
        await stream.writeDouble(this.ticksPerSecond);
        await stream.writeInt(this.updatesPerTick);
        await this.lootBoxSize.writeTo(stream);
        await this.unitSize.writeTo(stream);
        await stream.writeDouble(this.unitMaxHorizontalSpeed);
        await stream.writeDouble(this.unitFallSpeed);
        await stream.writeDouble(this.unitJumpTime);
        await stream.writeDouble(this.unitJumpSpeed);
        await stream.writeDouble(this.jumpPadJumpTime);
        await stream.writeDouble(this.jumpPadJumpSpeed);
        await stream.writeInt(this.unitMaxHealth);
        await stream.writeInt(this.healthPackHealth);
        await stream.writeInt(this.weaponParams.length);

        const weaponParamsKeys = Object.keys(this.weaponParams);
        const weaponParamsKeysSize = weaponParamsKeys.length;
        await stream.writeInt(weaponParamsKeysSize);
        for (let i = 0; i < weaponParamsKeysSize; i++) {
            let key = weaponParamsKeys[i];
            await stream.writeInt(key);
            await this.weaponParams[i].writeTo(stream);
        }

        await this.mineSize.writeTo(stream);
        await this.mineExplosionParams.writeTo(stream);
        await stream.writeDouble(this.minePrepareTime);
        await stream.writeDouble(this.mineTriggerTime);
        await stream.writeDouble(this.mineTriggerRadius);
        await stream.writeInt(this.killScore);
    }

    toString () {
        return 'Properties(' +
            this.maxTickCount + ',' +
            this.teamSize + ',' +
            this.ticksPerSecond + ',' +
            this.updatesPerTick + ',' +
            this.lootBoxSize + ',' +
            this.unitSize + ',' +
            this.unitMaxHorizontalSpeed + ',' +
            this.unitFallSpeed + ',' +
            this.unitJumpTime + ',' +
            this.unitJumpSpeed + ',' +
            this.jumpPadJumpTime + ',' +
            this.jumpPadJumpSpeed + ',' +
            this.unitMaxHealth + ',' +
            this.healthPackHealth + ',' +
            this.weaponParams + ',' +
            this.mineSize + ',' +
            this.mineExplosionParams + ',' +
            this.minePrepareTime + ',' +
            this.mineTriggerTime + ',' +
            this.mineTriggerRadius + ',' +
            this.killScore +
            ')';
    }
}

module.exports.Properties = Properties;
