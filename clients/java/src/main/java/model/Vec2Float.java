package model;

import util.StreamUtil;

public class Vec2Float {
    private float x;
    public float getX() { return x; }
    public void setX(float x) { this.x = x; }
    private float y;
    public float getY() { return y; }
    public void setY(float y) { this.y = y; }
    public Vec2Float() {}
    public Vec2Float(float x, float y) {
        this.x = x;
        this.y = y;
    }
    public static Vec2Float readFrom(java.io.InputStream stream) throws java.io.IOException {
        Vec2Float result = new Vec2Float();
        result.x = StreamUtil.readFloat(stream);
        result.y = StreamUtil.readFloat(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeFloat(stream, x);
        StreamUtil.writeFloat(stream, y);
    }
}
