package model;

import util.StreamUtil;

public class Properties {
    private int maxTickCount;
    public int getMaxTickCount() { return maxTickCount; }
    public void setMaxTickCount(int maxTickCount) { this.maxTickCount = maxTickCount; }
    private int teamSize;
    public int getTeamSize() { return teamSize; }
    public void setTeamSize(int teamSize) { this.teamSize = teamSize; }
    private double ticksPerSecond;
    public double getTicksPerSecond() { return ticksPerSecond; }
    public void setTicksPerSecond(double ticksPerSecond) { this.ticksPerSecond = ticksPerSecond; }
    private int updatesPerTick;
    public int getUpdatesPerTick() { return updatesPerTick; }
    public void setUpdatesPerTick(int updatesPerTick) { this.updatesPerTick = updatesPerTick; }
    private model.Vec2Double lootBoxSize;
    public model.Vec2Double getLootBoxSize() { return lootBoxSize; }
    public void setLootBoxSize(model.Vec2Double lootBoxSize) { this.lootBoxSize = lootBoxSize; }
    private model.Vec2Double unitSize;
    public model.Vec2Double getUnitSize() { return unitSize; }
    public void setUnitSize(model.Vec2Double unitSize) { this.unitSize = unitSize; }
    private double unitMaxHorizontalSpeed;
    public double getUnitMaxHorizontalSpeed() { return unitMaxHorizontalSpeed; }
    public void setUnitMaxHorizontalSpeed(double unitMaxHorizontalSpeed) { this.unitMaxHorizontalSpeed = unitMaxHorizontalSpeed; }
    private double unitFallSpeed;
    public double getUnitFallSpeed() { return unitFallSpeed; }
    public void setUnitFallSpeed(double unitFallSpeed) { this.unitFallSpeed = unitFallSpeed; }
    private double unitJumpTime;
    public double getUnitJumpTime() { return unitJumpTime; }
    public void setUnitJumpTime(double unitJumpTime) { this.unitJumpTime = unitJumpTime; }
    private double unitJumpSpeed;
    public double getUnitJumpSpeed() { return unitJumpSpeed; }
    public void setUnitJumpSpeed(double unitJumpSpeed) { this.unitJumpSpeed = unitJumpSpeed; }
    private double jumpPadJumpTime;
    public double getJumpPadJumpTime() { return jumpPadJumpTime; }
    public void setJumpPadJumpTime(double jumpPadJumpTime) { this.jumpPadJumpTime = jumpPadJumpTime; }
    private double jumpPadJumpSpeed;
    public double getJumpPadJumpSpeed() { return jumpPadJumpSpeed; }
    public void setJumpPadJumpSpeed(double jumpPadJumpSpeed) { this.jumpPadJumpSpeed = jumpPadJumpSpeed; }
    private int unitMaxHealth;
    public int getUnitMaxHealth() { return unitMaxHealth; }
    public void setUnitMaxHealth(int unitMaxHealth) { this.unitMaxHealth = unitMaxHealth; }
    private int healthPackHealth;
    public int getHealthPackHealth() { return healthPackHealth; }
    public void setHealthPackHealth(int healthPackHealth) { this.healthPackHealth = healthPackHealth; }
    private java.util.Map<model.WeaponType, model.WeaponParams> weaponParams;
    public java.util.Map<model.WeaponType, model.WeaponParams> getWeaponParams() { return weaponParams; }
    public void setWeaponParams(java.util.Map<model.WeaponType, model.WeaponParams> weaponParams) { this.weaponParams = weaponParams; }
    private model.Vec2Double mineSize;
    public model.Vec2Double getMineSize() { return mineSize; }
    public void setMineSize(model.Vec2Double mineSize) { this.mineSize = mineSize; }
    private model.ExplosionParams mineExplosionParams;
    public model.ExplosionParams getMineExplosionParams() { return mineExplosionParams; }
    public void setMineExplosionParams(model.ExplosionParams mineExplosionParams) { this.mineExplosionParams = mineExplosionParams; }
    private double minePrepareTime;
    public double getMinePrepareTime() { return minePrepareTime; }
    public void setMinePrepareTime(double minePrepareTime) { this.minePrepareTime = minePrepareTime; }
    private double mineTriggerTime;
    public double getMineTriggerTime() { return mineTriggerTime; }
    public void setMineTriggerTime(double mineTriggerTime) { this.mineTriggerTime = mineTriggerTime; }
    private double mineTriggerRadius;
    public double getMineTriggerRadius() { return mineTriggerRadius; }
    public void setMineTriggerRadius(double mineTriggerRadius) { this.mineTriggerRadius = mineTriggerRadius; }
    private int killScore;
    public int getKillScore() { return killScore; }
    public void setKillScore(int killScore) { this.killScore = killScore; }
    public Properties() {}
    public Properties(int maxTickCount, int teamSize, double ticksPerSecond, int updatesPerTick, model.Vec2Double lootBoxSize, model.Vec2Double unitSize, double unitMaxHorizontalSpeed, double unitFallSpeed, double unitJumpTime, double unitJumpSpeed, double jumpPadJumpTime, double jumpPadJumpSpeed, int unitMaxHealth, int healthPackHealth, java.util.Map<model.WeaponType, model.WeaponParams> weaponParams, model.Vec2Double mineSize, model.ExplosionParams mineExplosionParams, double minePrepareTime, double mineTriggerTime, double mineTriggerRadius, int killScore) {
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
    public static Properties readFrom(java.io.InputStream stream) throws java.io.IOException {
        Properties result = new Properties();
        result.maxTickCount = StreamUtil.readInt(stream);
        result.teamSize = StreamUtil.readInt(stream);
        result.ticksPerSecond = StreamUtil.readDouble(stream);
        result.updatesPerTick = StreamUtil.readInt(stream);
        result.lootBoxSize = model.Vec2Double.readFrom(stream);
        result.unitSize = model.Vec2Double.readFrom(stream);
        result.unitMaxHorizontalSpeed = StreamUtil.readDouble(stream);
        result.unitFallSpeed = StreamUtil.readDouble(stream);
        result.unitJumpTime = StreamUtil.readDouble(stream);
        result.unitJumpSpeed = StreamUtil.readDouble(stream);
        result.jumpPadJumpTime = StreamUtil.readDouble(stream);
        result.jumpPadJumpSpeed = StreamUtil.readDouble(stream);
        result.unitMaxHealth = StreamUtil.readInt(stream);
        result.healthPackHealth = StreamUtil.readInt(stream);
        int weaponParamsSize = StreamUtil.readInt(stream);
        result.weaponParams = new java.util.HashMap<>(weaponParamsSize);
        for (int i = 0; i < weaponParamsSize; i++) {
            model.WeaponType weaponParamsKey;
            switch (StreamUtil.readInt(stream)) {
            case 0:
                weaponParamsKey = model.WeaponType.PISTOL;
                break;
            case 1:
                weaponParamsKey = model.WeaponType.ASSAULT_RIFLE;
                break;
            case 2:
                weaponParamsKey = model.WeaponType.ROCKET_LAUNCHER;
                break;
            default:
                throw new java.io.IOException("Unexpected discriminant value");
            }
            model.WeaponParams weaponParamsValue;
            weaponParamsValue = model.WeaponParams.readFrom(stream);
            result.weaponParams.put(weaponParamsKey, weaponParamsValue);
        }
        result.mineSize = model.Vec2Double.readFrom(stream);
        result.mineExplosionParams = model.ExplosionParams.readFrom(stream);
        result.minePrepareTime = StreamUtil.readDouble(stream);
        result.mineTriggerTime = StreamUtil.readDouble(stream);
        result.mineTriggerRadius = StreamUtil.readDouble(stream);
        result.killScore = StreamUtil.readInt(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, maxTickCount);
        StreamUtil.writeInt(stream, teamSize);
        StreamUtil.writeDouble(stream, ticksPerSecond);
        StreamUtil.writeInt(stream, updatesPerTick);
        lootBoxSize.writeTo(stream);
        unitSize.writeTo(stream);
        StreamUtil.writeDouble(stream, unitMaxHorizontalSpeed);
        StreamUtil.writeDouble(stream, unitFallSpeed);
        StreamUtil.writeDouble(stream, unitJumpTime);
        StreamUtil.writeDouble(stream, unitJumpSpeed);
        StreamUtil.writeDouble(stream, jumpPadJumpTime);
        StreamUtil.writeDouble(stream, jumpPadJumpSpeed);
        StreamUtil.writeInt(stream, unitMaxHealth);
        StreamUtil.writeInt(stream, healthPackHealth);
        StreamUtil.writeInt(stream, weaponParams.size());
        for (java.util.Map.Entry<model.WeaponType, model.WeaponParams> weaponParamsEntry : weaponParams.entrySet()) {
            model.WeaponType weaponParamsKey = weaponParamsEntry.getKey();
            model.WeaponParams weaponParamsValue = weaponParamsEntry.getValue();
            StreamUtil.writeInt(stream, weaponParamsKey.discriminant);
            weaponParamsValue.writeTo(stream);
        }
        mineSize.writeTo(stream);
        mineExplosionParams.writeTo(stream);
        StreamUtil.writeDouble(stream, minePrepareTime);
        StreamUtil.writeDouble(stream, mineTriggerTime);
        StreamUtil.writeDouble(stream, mineTriggerRadius);
        StreamUtil.writeInt(stream, killScore);
    }
}
