const Vec2Double = require('./vec2-double').Vec2Double;
const WeaponParams = require('./weapon-params').WeaponParams;
const ExplosionParams = require('./explosion-params').ExplosionParams;
class Properties {
    constructor(maxTickCount, teamSize, ticksPerSecond, updatesPerTick, lootBoxSize, unitSize, unitMaxHorizontalSpeed, unitFallSpeed, unitJumpTime, unitJumpSpeed, jumpPadJumpTime, jumpPadJumpSpeed, unitMaxHealth, healthPackHealth, weaponParams, mineSize, mineExplosionParams, minePrepareTime, mineTriggerTime, mineTriggerRadius, killScore) {
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
    static async readFrom(stream) {
        let maxTickCount;
        maxTickCount = await stream.readInt();
        let teamSize;
        teamSize = await stream.readInt();
        let ticksPerSecond;
        ticksPerSecond = await stream.readDouble();
        let updatesPerTick;
        updatesPerTick = await stream.readInt();
        let lootBoxSize;
        lootBoxSize = await Vec2Double.readFrom(stream);
        let unitSize;
        unitSize = await Vec2Double.readFrom(stream);
        let unitMaxHorizontalSpeed;
        unitMaxHorizontalSpeed = await stream.readDouble();
        let unitFallSpeed;
        unitFallSpeed = await stream.readDouble();
        let unitJumpTime;
        unitJumpTime = await stream.readDouble();
        let unitJumpSpeed;
        unitJumpSpeed = await stream.readDouble();
        let jumpPadJumpTime;
        jumpPadJumpTime = await stream.readDouble();
        let jumpPadJumpSpeed;
        jumpPadJumpSpeed = await stream.readDouble();
        let unitMaxHealth;
        unitMaxHealth = await stream.readInt();
        let healthPackHealth;
        healthPackHealth = await stream.readInt();
        let weaponParams;
        weaponParams = new Map();
        for (let i = await stream.readInt(); i > 0; i--) {
            let weaponParamsKey;
            let weaponParamsValue;
            weaponParamsKey = stream.readInt();
            weaponParamsValue = await WeaponParams.readFrom(stream);
            weaponParams.set(weaponParamsKey, weaponParamsValue);
        }
        let mineSize;
        mineSize = await Vec2Double.readFrom(stream);
        let mineExplosionParams;
        mineExplosionParams = await ExplosionParams.readFrom(stream);
        let minePrepareTime;
        minePrepareTime = await stream.readDouble();
        let mineTriggerTime;
        mineTriggerTime = await stream.readDouble();
        let mineTriggerRadius;
        mineTriggerRadius = await stream.readDouble();
        let killScore;
        killScore = await stream.readInt();
        return new Properties(maxTickCount, teamSize, ticksPerSecond, updatesPerTick, lootBoxSize, unitSize, unitMaxHorizontalSpeed, unitFallSpeed, unitJumpTime, unitJumpSpeed, jumpPadJumpTime, jumpPadJumpSpeed, unitMaxHealth, healthPackHealth, weaponParams, mineSize, mineExplosionParams, minePrepareTime, mineTriggerTime, mineTriggerRadius, killScore);
    }
    async writeTo(stream) {
        let maxTickCount = this.maxTickCount;
        await stream.writeInt(maxTickCount);
        let teamSize = this.teamSize;
        await stream.writeInt(teamSize);
        let ticksPerSecond = this.ticksPerSecond;
        await stream.writeDouble(ticksPerSecond);
        let updatesPerTick = this.updatesPerTick;
        await stream.writeInt(updatesPerTick);
        let lootBoxSize = this.lootBoxSize;
        await lootBoxSize.writeTo(stream);
        let unitSize = this.unitSize;
        await unitSize.writeTo(stream);
        let unitMaxHorizontalSpeed = this.unitMaxHorizontalSpeed;
        await stream.writeDouble(unitMaxHorizontalSpeed);
        let unitFallSpeed = this.unitFallSpeed;
        await stream.writeDouble(unitFallSpeed);
        let unitJumpTime = this.unitJumpTime;
        await stream.writeDouble(unitJumpTime);
        let unitJumpSpeed = this.unitJumpSpeed;
        await stream.writeDouble(unitJumpSpeed);
        let jumpPadJumpTime = this.jumpPadJumpTime;
        await stream.writeDouble(jumpPadJumpTime);
        let jumpPadJumpSpeed = this.jumpPadJumpSpeed;
        await stream.writeDouble(jumpPadJumpSpeed);
        let unitMaxHealth = this.unitMaxHealth;
        await stream.writeInt(unitMaxHealth);
        let healthPackHealth = this.healthPackHealth;
        await stream.writeInt(healthPackHealth);
        let weaponParams = this.weaponParams;
        await stream.writeInt(weaponParams.size);
        for (let weaponParamsEntry of weaponParams) {
            let weaponParamsKey = weaponParamsEntry[0];
            let weaponParamsValue = weaponParamsEntry[1];
            await stream.writeInt(weaponParamsKey);
            await weaponParamsValue.writeTo(stream);
        }
        let mineSize = this.mineSize;
        await mineSize.writeTo(stream);
        let mineExplosionParams = this.mineExplosionParams;
        await mineExplosionParams.writeTo(stream);
        let minePrepareTime = this.minePrepareTime;
        await stream.writeDouble(minePrepareTime);
        let mineTriggerTime = this.mineTriggerTime;
        await stream.writeDouble(mineTriggerTime);
        let mineTriggerRadius = this.mineTriggerRadius;
        await stream.writeDouble(mineTriggerRadius);
        let killScore = this.killScore;
        await stream.writeInt(killScore);
    }
}
module.exports = { Properties: Properties }
