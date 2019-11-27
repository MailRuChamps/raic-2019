package model;

import util.StreamUtil;

public class Bullet {
    private model.WeaponType weaponType;
    public model.WeaponType getWeaponType() { return weaponType; }
    public void setWeaponType(model.WeaponType weaponType) { this.weaponType = weaponType; }
    private int unitId;
    public int getUnitId() { return unitId; }
    public void setUnitId(int unitId) { this.unitId = unitId; }
    private int playerId;
    public int getPlayerId() { return playerId; }
    public void setPlayerId(int playerId) { this.playerId = playerId; }
    private model.Vec2Double position;
    public model.Vec2Double getPosition() { return position; }
    public void setPosition(model.Vec2Double position) { this.position = position; }
    private model.Vec2Double velocity;
    public model.Vec2Double getVelocity() { return velocity; }
    public void setVelocity(model.Vec2Double velocity) { this.velocity = velocity; }
    private int damage;
    public int getDamage() { return damage; }
    public void setDamage(int damage) { this.damage = damage; }
    private double size;
    public double getSize() { return size; }
    public void setSize(double size) { this.size = size; }
    private model.ExplosionParams explosionParams;
    public model.ExplosionParams getExplosionParams() { return explosionParams; }
    public void setExplosionParams(model.ExplosionParams explosionParams) { this.explosionParams = explosionParams; }
    public Bullet() {}
    public Bullet(model.WeaponType weaponType, int unitId, int playerId, model.Vec2Double position, model.Vec2Double velocity, int damage, double size, model.ExplosionParams explosionParams) {
        this.weaponType = weaponType;
        this.unitId = unitId;
        this.playerId = playerId;
        this.position = position;
        this.velocity = velocity;
        this.damage = damage;
        this.size = size;
        this.explosionParams = explosionParams;
    }
    public static Bullet readFrom(java.io.InputStream stream) throws java.io.IOException {
        Bullet result = new Bullet();
        switch (StreamUtil.readInt(stream)) {
        case 0:
            result.weaponType = model.WeaponType.PISTOL;
            break;
        case 1:
            result.weaponType = model.WeaponType.ASSAULT_RIFLE;
            break;
        case 2:
            result.weaponType = model.WeaponType.ROCKET_LAUNCHER;
            break;
        default:
            throw new java.io.IOException("Unexpected discriminant value");
        }
        result.unitId = StreamUtil.readInt(stream);
        result.playerId = StreamUtil.readInt(stream);
        result.position = model.Vec2Double.readFrom(stream);
        result.velocity = model.Vec2Double.readFrom(stream);
        result.damage = StreamUtil.readInt(stream);
        result.size = StreamUtil.readDouble(stream);
        if (StreamUtil.readBoolean(stream)) {
            result.explosionParams = model.ExplosionParams.readFrom(stream);
        } else {
            result.explosionParams = null;
        }
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, weaponType.discriminant);
        StreamUtil.writeInt(stream, unitId);
        StreamUtil.writeInt(stream, playerId);
        position.writeTo(stream);
        velocity.writeTo(stream);
        StreamUtil.writeInt(stream, damage);
        StreamUtil.writeDouble(stream, size);
        if (explosionParams == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            explosionParams.writeTo(stream);
        }
    }
}
