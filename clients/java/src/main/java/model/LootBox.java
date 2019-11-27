package model;

import util.StreamUtil;

public class LootBox {
    private model.Vec2Double position;
    public model.Vec2Double getPosition() { return position; }
    public void setPosition(model.Vec2Double position) { this.position = position; }
    private model.Vec2Double size;
    public model.Vec2Double getSize() { return size; }
    public void setSize(model.Vec2Double size) { this.size = size; }
    private model.Item item;
    public model.Item getItem() { return item; }
    public void setItem(model.Item item) { this.item = item; }
    public LootBox() {}
    public LootBox(model.Vec2Double position, model.Vec2Double size, model.Item item) {
        this.position = position;
        this.size = size;
        this.item = item;
    }
    public static LootBox readFrom(java.io.InputStream stream) throws java.io.IOException {
        LootBox result = new LootBox();
        result.position = model.Vec2Double.readFrom(stream);
        result.size = model.Vec2Double.readFrom(stream);
        result.item = model.Item.readFrom(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        position.writeTo(stream);
        size.writeTo(stream);
        item.writeTo(stream);
    }
}
