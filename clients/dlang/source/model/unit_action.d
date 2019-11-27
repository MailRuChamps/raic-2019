import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct UnitAction {
    double velocity;
    bool jump;
    bool jumpDown;
    Vec2Double aim;
    bool shoot;
    bool swapWeapon;
    bool plantMine;
    this(double velocity, bool jump, bool jumpDown, Vec2Double aim, bool shoot, bool swapWeapon, bool plantMine) {
        this.velocity = velocity;
        this.jump = jump;
        this.jumpDown = jumpDown;
        this.aim = aim;
        this.shoot = shoot;
        this.swapWeapon = swapWeapon;
        this.plantMine = plantMine;
    }
    static UnitAction readFrom(Stream reader) {
        auto result = UnitAction();
        result.velocity = reader.readDouble();
        result.jump = reader.readBool();
        result.jumpDown = reader.readBool();
        result.aim = Vec2Double.readFrom(reader);
        result.shoot = reader.readBool();
        result.swapWeapon = reader.readBool();
        result.plantMine = reader.readBool();
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(velocity);
        writer.write(jump);
        writer.write(jumpDown);
        aim.writeTo(writer);
        writer.write(shoot);
        writer.write(swapWeapon);
        writer.write(plantMine);
    }
    string toString() const {
        return "UnitAction" ~ "(" ~
            to!string(velocity) ~
            to!string(jump) ~
            to!string(jumpDown) ~
            to!string(aim) ~
            to!string(shoot) ~
            to!string(swapWeapon) ~
            to!string(plantMine) ~
            ")";
    }
}
