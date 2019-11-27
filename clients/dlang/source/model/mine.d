import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Mine {
    int playerId;
    Vec2Double position;
    Vec2Double size;
    MineState state;
    Nullable!(double) timer;
    double triggerRadius;
    ExplosionParameters explosionParameters;
    this(int playerId, Vec2Double position, Vec2Double size, MineState state, Nullable!(double) timer, double triggerRadius, ExplosionParameters explosionParameters) {
        this.playerId = playerId;
        this.position = position;
        this.size = size;
        this.state = state;
        this.timer = timer;
        this.triggerRadius = triggerRadius;
        this.explosionParameters = explosionParameters;
    }
    static Mine readFrom(Stream reader) {
        auto result = Mine();
        result.playerId = reader.readInt();
        result.position = Vec2Double.readFrom(reader);
        result.size = Vec2Double.readFrom(reader);
        switch (reader.readInt()) {
        case 0:
            result.state = MineState.Preparing;
            break;
        case 1:
            result.state = MineState.Idle;
            break;
        case 2:
            result.state = MineState.Triggered;
            break;
        case 3:
            result.state = MineState.Exploded;
            break;
        default:
            throw new Exception("Unexpected discriminant value");
        }
        if (reader.readBool()) {
            result.timer = reader.readDouble();
        } else {
            result.timer.nullify();
        }
        result.triggerRadius = reader.readDouble();
        result.explosionParameters = ExplosionParameters.readFrom(reader);
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(playerId);
        position.writeTo(writer);
        size.writeTo(writer);
        writer.write(cast(int)(state));
        if (timer.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(timer.get);
        }
        writer.write(triggerRadius);
        explosionParameters.writeTo(writer);
    }
    string toString() const {
        return "Mine" ~ "(" ~
            to!string(playerId) ~
            to!string(position) ~
            to!string(size) ~
            to!string(state) ~
            to!string(timer) ~
            to!string(triggerRadius) ~
            to!string(explosionParameters) ~
            ")";
    }
}
