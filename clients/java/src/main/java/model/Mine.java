package model;

import util.StreamUtil;

public class Mine {
    private int playerId;
    public int getPlayerId() { return playerId; }
    public void setPlayerId(int playerId) { this.playerId = playerId; }
    private model.Vec2Double position;
    public model.Vec2Double getPosition() { return position; }
    public void setPosition(model.Vec2Double position) { this.position = position; }
    private model.Vec2Double size;
    public model.Vec2Double getSize() { return size; }
    public void setSize(model.Vec2Double size) { this.size = size; }
    private model.MineState state;
    public model.MineState getState() { return state; }
    public void setState(model.MineState state) { this.state = state; }
    private Double timer;
    public Double getTimer() { return timer; }
    public void setTimer(Double timer) { this.timer = timer; }
    private double triggerRadius;
    public double getTriggerRadius() { return triggerRadius; }
    public void setTriggerRadius(double triggerRadius) { this.triggerRadius = triggerRadius; }
    private model.ExplosionParams explosionParams;
    public model.ExplosionParams getExplosionParams() { return explosionParams; }
    public void setExplosionParams(model.ExplosionParams explosionParams) { this.explosionParams = explosionParams; }
    public Mine() {}
    public Mine(int playerId, model.Vec2Double position, model.Vec2Double size, model.MineState state, Double timer, double triggerRadius, model.ExplosionParams explosionParams) {
        this.playerId = playerId;
        this.position = position;
        this.size = size;
        this.state = state;
        this.timer = timer;
        this.triggerRadius = triggerRadius;
        this.explosionParams = explosionParams;
    }
    public static Mine readFrom(java.io.InputStream stream) throws java.io.IOException {
        Mine result = new Mine();
        result.playerId = StreamUtil.readInt(stream);
        result.position = model.Vec2Double.readFrom(stream);
        result.size = model.Vec2Double.readFrom(stream);
        switch (StreamUtil.readInt(stream)) {
        case 0:
            result.state = model.MineState.PREPARING;
            break;
        case 1:
            result.state = model.MineState.IDLE;
            break;
        case 2:
            result.state = model.MineState.TRIGGERED;
            break;
        case 3:
            result.state = model.MineState.EXPLODED;
            break;
        default:
            throw new java.io.IOException("Unexpected discriminant value");
        }
        if (StreamUtil.readBoolean(stream)) {
            result.timer = StreamUtil.readDouble(stream);
        } else {
            result.timer = null;
        }
        result.triggerRadius = StreamUtil.readDouble(stream);
        result.explosionParams = model.ExplosionParams.readFrom(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, playerId);
        position.writeTo(stream);
        size.writeTo(stream);
        StreamUtil.writeInt(stream, state.discriminant);
        if (timer == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeDouble(stream, timer);
        }
        StreamUtil.writeDouble(stream, triggerRadius);
        explosionParams.writeTo(stream);
    }
}
