package model;

import util.StreamUtil;

public class WeaponParams {
    private int magazineSize;
    public int getMagazineSize() { return magazineSize; }
    public void setMagazineSize(int magazineSize) { this.magazineSize = magazineSize; }
    private double fireRate;
    public double getFireRate() { return fireRate; }
    public void setFireRate(double fireRate) { this.fireRate = fireRate; }
    private double reloadTime;
    public double getReloadTime() { return reloadTime; }
    public void setReloadTime(double reloadTime) { this.reloadTime = reloadTime; }
    private double minSpread;
    public double getMinSpread() { return minSpread; }
    public void setMinSpread(double minSpread) { this.minSpread = minSpread; }
    private double maxSpread;
    public double getMaxSpread() { return maxSpread; }
    public void setMaxSpread(double maxSpread) { this.maxSpread = maxSpread; }
    private double recoil;
    public double getRecoil() { return recoil; }
    public void setRecoil(double recoil) { this.recoil = recoil; }
    private double aimSpeed;
    public double getAimSpeed() { return aimSpeed; }
    public void setAimSpeed(double aimSpeed) { this.aimSpeed = aimSpeed; }
    private model.BulletParams bullet;
    public model.BulletParams getBullet() { return bullet; }
    public void setBullet(model.BulletParams bullet) { this.bullet = bullet; }
    private model.ExplosionParams explosion;
    public model.ExplosionParams getExplosion() { return explosion; }
    public void setExplosion(model.ExplosionParams explosion) { this.explosion = explosion; }
    public WeaponParams() {}
    public WeaponParams(int magazineSize, double fireRate, double reloadTime, double minSpread, double maxSpread, double recoil, double aimSpeed, model.BulletParams bullet, model.ExplosionParams explosion) {
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
    public static WeaponParams readFrom(java.io.InputStream stream) throws java.io.IOException {
        WeaponParams result = new WeaponParams();
        result.magazineSize = StreamUtil.readInt(stream);
        result.fireRate = StreamUtil.readDouble(stream);
        result.reloadTime = StreamUtil.readDouble(stream);
        result.minSpread = StreamUtil.readDouble(stream);
        result.maxSpread = StreamUtil.readDouble(stream);
        result.recoil = StreamUtil.readDouble(stream);
        result.aimSpeed = StreamUtil.readDouble(stream);
        result.bullet = model.BulletParams.readFrom(stream);
        if (StreamUtil.readBoolean(stream)) {
            result.explosion = model.ExplosionParams.readFrom(stream);
        } else {
            result.explosion = null;
        }
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, magazineSize);
        StreamUtil.writeDouble(stream, fireRate);
        StreamUtil.writeDouble(stream, reloadTime);
        StreamUtil.writeDouble(stream, minSpread);
        StreamUtil.writeDouble(stream, maxSpread);
        StreamUtil.writeDouble(stream, recoil);
        StreamUtil.writeDouble(stream, aimSpeed);
        bullet.writeTo(stream);
        if (explosion == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            explosion.writeTo(stream);
        }
    }
}
