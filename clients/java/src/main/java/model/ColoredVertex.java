package model;

import util.StreamUtil;

public class ColoredVertex {
    private model.Vec2Float position;
    public model.Vec2Float getPosition() { return position; }
    public void setPosition(model.Vec2Float position) { this.position = position; }
    private model.ColorFloat color;
    public model.ColorFloat getColor() { return color; }
    public void setColor(model.ColorFloat color) { this.color = color; }
    public ColoredVertex() {}
    public ColoredVertex(model.Vec2Float position, model.ColorFloat color) {
        this.position = position;
        this.color = color;
    }
    public static ColoredVertex readFrom(java.io.InputStream stream) throws java.io.IOException {
        ColoredVertex result = new ColoredVertex();
        result.position = model.Vec2Float.readFrom(stream);
        result.color = model.ColorFloat.readFrom(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        position.writeTo(stream);
        color.writeTo(stream);
    }
}
