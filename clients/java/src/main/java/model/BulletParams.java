package model;

import util.StreamUtil;

public class BulletParams {
    private double speed;
    public double getSpeed() { return speed; }
    public void setSpeed(double speed) { this.speed = speed; }
    private double size;
    public double getSize() { return size; }
    public void setSize(double size) { this.size = size; }
    private int damage;
    public int getDamage() { return damage; }
    public void setDamage(int damage) { this.damage = damage; }
    public BulletParams() {}
    public BulletParams(double speed, double size, int damage) {
        this.speed = speed;
        this.size = size;
        this.damage = damage;
    }
    public static BulletParams readFrom(java.io.InputStream stream) throws java.io.IOException {
        BulletParams result = new BulletParams();
        result.speed = StreamUtil.readDouble(stream);
        result.size = StreamUtil.readDouble(stream);
        result.damage = StreamUtil.readInt(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeDouble(stream, speed);
        StreamUtil.writeDouble(stream, size);
        StreamUtil.writeInt(stream, damage);
    }
}
