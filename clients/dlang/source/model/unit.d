import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Unit {
    int playerId;
    int id;
    int health;
    Vec2Double position;
    Vec2Double size;
    JumpState jumpState;
    bool walkedRight;
    bool stand;
    bool onGround;
    bool onLadder;
    int mines;
    Nullable!(Weapon) weapon;
    this(int playerId, int id, int health, Vec2Double position, Vec2Double size, JumpState jumpState, bool walkedRight, bool stand, bool onGround, bool onLadder, int mines, Nullable!(Weapon) weapon) {
        this.playerId = playerId;
        this.id = id;
        this.health = health;
        this.position = position;
        this.size = size;
        this.jumpState = jumpState;
        this.walkedRight = walkedRight;
        this.stand = stand;
        this.onGround = onGround;
        this.onLadder = onLadder;
        this.mines = mines;
        this.weapon = weapon;
    }
    static Unit readFrom(Stream reader) {
        auto result = Unit();
        result.playerId = reader.readInt();
        result.id = reader.readInt();
        result.health = reader.readInt();
        result.position = Vec2Double.readFrom(reader);
        result.size = Vec2Double.readFrom(reader);
        result.jumpState = JumpState.readFrom(reader);
        result.walkedRight = reader.readBool();
        result.stand = reader.readBool();
        result.onGround = reader.readBool();
        result.onLadder = reader.readBool();
        result.mines = reader.readInt();
        if (reader.readBool()) {
            result.weapon = Weapon.readFrom(reader);
        } else {
            result.weapon.nullify();
        }
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(playerId);
        writer.write(id);
        writer.write(health);
        position.writeTo(writer);
        size.writeTo(writer);
        jumpState.writeTo(writer);
        writer.write(walkedRight);
        writer.write(stand);
        writer.write(onGround);
        writer.write(onLadder);
        writer.write(mines);
        if (weapon.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            weapon.get.writeTo(writer);
        }
    }
    string toString() const {
        return "Unit" ~ "(" ~
            to!string(playerId) ~
            to!string(id) ~
            to!string(health) ~
            to!string(position) ~
            to!string(size) ~
            to!string(jumpState) ~
            to!string(walkedRight) ~
            to!string(stand) ~
            to!string(onGround) ~
            to!string(onLadder) ~
            to!string(mines) ~
            to!string(weapon) ~
            ")";
    }
}
