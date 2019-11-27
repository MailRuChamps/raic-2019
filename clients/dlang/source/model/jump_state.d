import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct JumpState {
    bool canJump;
    double speed;
    double maxTime;
    bool canCancel;
    this(bool canJump, double speed, double maxTime, bool canCancel) {
        this.canJump = canJump;
        this.speed = speed;
        this.maxTime = maxTime;
        this.canCancel = canCancel;
    }
    static JumpState readFrom(Stream reader) {
        auto result = JumpState();
        result.canJump = reader.readBool();
        result.speed = reader.readDouble();
        result.maxTime = reader.readDouble();
        result.canCancel = reader.readBool();
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(canJump);
        writer.write(speed);
        writer.write(maxTime);
        writer.write(canCancel);
    }
    string toString() const {
        return "JumpState" ~ "(" ~
            to!string(canJump) ~
            to!string(speed) ~
            to!string(maxTime) ~
            to!string(canCancel) ~
            ")";
    }
}
