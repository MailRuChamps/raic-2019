package model;

import util.StreamUtil;

public class ColorFloat {
    private float r;
    public float getR() { return r; }
    public void setR(float r) { this.r = r; }
    private float g;
    public float getG() { return g; }
    public void setG(float g) { this.g = g; }
    private float b;
    public float getB() { return b; }
    public void setB(float b) { this.b = b; }
    private float a;
    public float getA() { return a; }
    public void setA(float a) { this.a = a; }
    public ColorFloat() {}
    public ColorFloat(float r, float g, float b, float a) {
        this.r = r;
        this.g = g;
        this.b = b;
        this.a = a;
    }
    public static ColorFloat readFrom(java.io.InputStream stream) throws java.io.IOException {
        ColorFloat result = new ColorFloat();
        result.r = StreamUtil.readFloat(stream);
        result.g = StreamUtil.readFloat(stream);
        result.b = StreamUtil.readFloat(stream);
        result.a = StreamUtil.readFloat(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeFloat(stream, r);
        StreamUtil.writeFloat(stream, g);
        StreamUtil.writeFloat(stream, b);
        StreamUtil.writeFloat(stream, a);
    }
}
