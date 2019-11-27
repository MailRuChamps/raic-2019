package model;

import util.StreamUtil;

public class JumpState {
    private boolean canJump;
    public boolean isCanJump() { return canJump; }
    public void setCanJump(boolean canJump) { this.canJump = canJump; }
    private double speed;
    public double getSpeed() { return speed; }
    public void setSpeed(double speed) { this.speed = speed; }
    private double maxTime;
    public double getMaxTime() { return maxTime; }
    public void setMaxTime(double maxTime) { this.maxTime = maxTime; }
    private boolean canCancel;
    public boolean isCanCancel() { return canCancel; }
    public void setCanCancel(boolean canCancel) { this.canCancel = canCancel; }
    public JumpState() {}
    public JumpState(boolean canJump, double speed, double maxTime, boolean canCancel) {
        this.canJump = canJump;
        this.speed = speed;
        this.maxTime = maxTime;
        this.canCancel = canCancel;
    }
    public static JumpState readFrom(java.io.InputStream stream) throws java.io.IOException {
        JumpState result = new JumpState();
        result.canJump = StreamUtil.readBoolean(stream);
        result.speed = StreamUtil.readDouble(stream);
        result.maxTime = StreamUtil.readDouble(stream);
        result.canCancel = StreamUtil.readBoolean(stream);
        return result;
    }
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeBoolean(stream, canJump);
        StreamUtil.writeDouble(stream, speed);
        StreamUtil.writeDouble(stream, maxTime);
        StreamUtil.writeBoolean(stream, canCancel);
    }
}
