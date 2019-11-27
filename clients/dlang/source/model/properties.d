import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Properties {
    int maxTickCount;
    int teamSize;
    double ticksPerSecond;
    int updatesPerTick;
    Vec2Double lootBoxSize;
    Vec2Double unitSize;
    double unitMaxHorizontalSpeed;
    double unitFallSpeed;
    double unitJumpTime;
    double unitJumpSpeed;
    double jumpPadJumpTime;
    double jumpPadJumpSpeed;
    int unitMaxHealth;
    int healthPackHealth;
    WeaponParameters[WeaponType] weaponParameters;
    Vec2Double mineSize;
    ExplosionParameters mineExplosionParameters;
    double minePrepareTime;
    double mineTriggerTime;
    double mineTriggerRadius;
    int killScore;
    this(int maxTickCount, int teamSize, double ticksPerSecond, int updatesPerTick, Vec2Double lootBoxSize, Vec2Double unitSize, double unitMaxHorizontalSpeed, double unitFallSpeed, double unitJumpTime, double unitJumpSpeed, double jumpPadJumpTime, double jumpPadJumpSpeed, int unitMaxHealth, int healthPackHealth, WeaponParameters[WeaponType] weaponParameters, Vec2Double mineSize, ExplosionParameters mineExplosionParameters, double minePrepareTime, double mineTriggerTime, double mineTriggerRadius, int killScore) {
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
        this.weaponParameters = weaponParameters;
        this.mineSize = mineSize;
        this.mineExplosionParameters = mineExplosionParameters;
        this.minePrepareTime = minePrepareTime;
        this.mineTriggerTime = mineTriggerTime;
        this.mineTriggerRadius = mineTriggerRadius;
        this.killScore = killScore;
    }
    static Properties readFrom(Stream reader) {
        auto result = Properties();
        result.maxTickCount = reader.readInt();
        result.teamSize = reader.readInt();
        result.ticksPerSecond = reader.readDouble();
        result.updatesPerTick = reader.readInt();
        result.lootBoxSize = Vec2Double.readFrom(reader);
        result.unitSize = Vec2Double.readFrom(reader);
        result.unitMaxHorizontalSpeed = reader.readDouble();
        result.unitFallSpeed = reader.readDouble();
        result.unitJumpTime = reader.readDouble();
        result.unitJumpSpeed = reader.readDouble();
        result.jumpPadJumpTime = reader.readDouble();
        result.jumpPadJumpSpeed = reader.readDouble();
        result.unitMaxHealth = reader.readInt();
        result.healthPackHealth = reader.readInt();
        int weaponParametersSize = reader.readInt();
        result.weaponParameters.clear();
        for (int i = 0; i < weaponParametersSize; i++) {
            WeaponType weaponParametersKey;
            switch (reader.readInt()) {
            case 0:
                weaponParametersKey = WeaponType.Pistol;
                break;
            case 1:
                weaponParametersKey = WeaponType.AssaultRifle;
                break;
            case 2:
                weaponParametersKey = WeaponType.RocketLauncher;
                break;
            default:
                throw new Exception("Unexpected discriminant value");
            }
            WeaponParameters weaponParametersValue;
            weaponParametersValue = WeaponParameters.readFrom(reader);
            result.weaponParameters[weaponParametersKey] = weaponParametersValue;
        }
        result.mineSize = Vec2Double.readFrom(reader);
        result.mineExplosionParameters = ExplosionParameters.readFrom(reader);
        result.minePrepareTime = reader.readDouble();
        result.mineTriggerTime = reader.readDouble();
        result.mineTriggerRadius = reader.readDouble();
        result.killScore = reader.readInt();
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(maxTickCount);
        writer.write(teamSize);
        writer.write(ticksPerSecond);
        writer.write(updatesPerTick);
        lootBoxSize.writeTo(writer);
        unitSize.writeTo(writer);
        writer.write(unitMaxHorizontalSpeed);
        writer.write(unitFallSpeed);
        writer.write(unitJumpTime);
        writer.write(unitJumpSpeed);
        writer.write(jumpPadJumpTime);
        writer.write(jumpPadJumpSpeed);
        writer.write(unitMaxHealth);
        writer.write(healthPackHealth);
        writer.write(cast(int)(weaponParameters.length));
        foreach (weaponParametersKey, weaponParametersValue; weaponParameters) {
            writer.write(cast(int)(weaponParametersKey));
            weaponParametersValue.writeTo(writer);
        }
        mineSize.writeTo(writer);
        mineExplosionParameters.writeTo(writer);
        writer.write(minePrepareTime);
        writer.write(mineTriggerTime);
        writer.write(mineTriggerRadius);
        writer.write(killScore);
    }
    string toString() const {
        return "Properties" ~ "(" ~
            to!string(maxTickCount) ~
            to!string(teamSize) ~
            to!string(ticksPerSecond) ~
            to!string(updatesPerTick) ~
            to!string(lootBoxSize) ~
            to!string(unitSize) ~
            to!string(unitMaxHorizontalSpeed) ~
            to!string(unitFallSpeed) ~
            to!string(unitJumpTime) ~
            to!string(unitJumpSpeed) ~
            to!string(jumpPadJumpTime) ~
            to!string(jumpPadJumpSpeed) ~
            to!string(unitMaxHealth) ~
            to!string(healthPackHealth) ~
            to!string(weaponParameters) ~
            to!string(mineSize) ~
            to!string(mineExplosionParameters) ~
            to!string(minePrepareTime) ~
            to!string(mineTriggerTime) ~
            to!string(mineTriggerRadius) ~
            to!string(killScore) ~
            ")";
    }
}
