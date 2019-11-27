package model;

import util.StreamUtil;

public class Weapon {
    private model.WeaponType typ;
    public model.WeaponType getTyp() { return typ; }
    public void setTyp(model.WeaponType typ) { this.typ = typ; }
    private model.WeaponParams params;
    public model.WeaponParams getParams() { return params; }
    public void setParams(model.WeaponParams params) { this.params = params; }
    private int magazine;
    public int getMagazine() { return magazine; }
    public void setMagazine(int magazine) { this.magazine = magazine; }
    private boolean wasShooting;
    public boolean isWasShooting() { return wasShooting; }
    public void setWasShooting(boolean wasShooting) { this.wasShooting = wasShooting; }
    private double spread;
    public double getSpread() { return spread; }
    public void setSpread(double spread) { this.spread = spread; }
    private Double fireTimer;
    public Double getFireTimer() { return fireTimer; }
    public void setFireTimer(Double fireTimer) { this.fireTimer = fireTimer; }
    private Double lastAngle;
    public Double getLastAngle() { return lastAngle; }
    public void setLastAngle(Double lastAngle) { this.lastAngle = lastAngle; }
    private Integer lastFireTick;
    public Integer getLastFireTick() { return lastFireTick; }
    public void setLastFireTick(Integer lastFireTick) { this.lastFireTick = lastFireTick; }
    public Weapon() {}
    public Weapon(model.WeaponType typ, model.WeaponParams params, int magazine, boolean wasShooting, double spread, Double fireTimer, Double lastAngle, Integer lastFireTick) {
        this.typ = typ;
        this.params = params;
        this.magazine = magazine;
        this.wasShooting = wasShooting;
        this.spread = spread;
        this.fireTimer = fireTimer;
        this.lastAngle = lastAngle;
        this.lastFireTick = lastFireTick;
    }
    public static Weapon readFrom(java.io.InputStream stream) throws java.io.IOException {
        Weapon result = new Weapon();
        switch (StreamUtil.readInt(stream)) {
        case 0:
            result.typ = model.WeaponType.PISTOL;
            break;
        case 1:
            result.typ = model.WeaponType.ASSAULT_RIFLE;
            break;
        case 2:
            result.typ = model.WeaponType.ROCKET_LAUNCHER;
            break;
        default:
            throw new java.io.IOException("Unexpected discriminant value");
        }
        result.params = model.WeaponParams.readFrom(stream);
        result.magazine = StreamUtil.readInt(stream);
        result.wasShooting = StreamUtil.readBoolean(stream);
        result.spread = StreamUtil.readDouble(stream);
        if (StreamUtil.readBoolean(stream)) {
            result.fireTimer = StreamUtil.readDouble(stream);
        } else {
            result.fireTimer = null;
        }
        if (StreamUtil.readBoolean(stream)) {
            result.lastAngle = StreamUtil.readDouble(stream);
        } else {
            result.lastAngle = null;
        }
        if (StreamUtil.readBoolean(stream)) {
            result.lastFireTick = StreamUtil.readInt(stream);
        } else {
            result.lastFireTick = null;
        }
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, typ.discriminant);
        params.writeTo(stream);
        StreamUtil.writeInt(stream, magazine);
        StreamUtil.writeBoolean(stream, wasShooting);
        StreamUtil.writeDouble(stream, spread);
        if (fireTimer == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeDouble(stream, fireTimer);
        }
        if (lastAngle == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeDouble(stream, lastAngle);
        }
        if (lastFireTick == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeInt(stream, lastFireTick);
        }
    }
}
