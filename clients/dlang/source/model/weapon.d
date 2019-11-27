import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Weapon {
    WeaponType typ;
    WeaponParameters parameters;
    int magazine;
    bool wasShooting;
    double spread;
    Nullable!(double) fireTimer;
    Nullable!(double) lastAngle;
    Nullable!(int) lastFireTick;
    this(WeaponType typ, WeaponParameters parameters, int magazine, bool wasShooting, double spread, Nullable!(double) fireTimer, Nullable!(double) lastAngle, Nullable!(int) lastFireTick) {
        this.typ = typ;
        this.parameters = parameters;
        this.magazine = magazine;
        this.wasShooting = wasShooting;
        this.spread = spread;
        this.fireTimer = fireTimer;
        this.lastAngle = lastAngle;
        this.lastFireTick = lastFireTick;
    }
    static Weapon readFrom(Stream reader) {
        auto result = Weapon();
        switch (reader.readInt()) {
        case 0:
            result.typ = WeaponType.Pistol;
            break;
        case 1:
            result.typ = WeaponType.AssaultRifle;
            break;
        case 2:
            result.typ = WeaponType.RocketLauncher;
            break;
        default:
            throw new Exception("Unexpected discriminant value");
        }
        result.parameters = WeaponParameters.readFrom(reader);
        result.magazine = reader.readInt();
        result.wasShooting = reader.readBool();
        result.spread = reader.readDouble();
        if (reader.readBool()) {
            result.fireTimer = reader.readDouble();
        } else {
            result.fireTimer.nullify();
        }
        if (reader.readBool()) {
            result.lastAngle = reader.readDouble();
        } else {
            result.lastAngle.nullify();
        }
        if (reader.readBool()) {
            result.lastFireTick = reader.readInt();
        } else {
            result.lastFireTick.nullify();
        }
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(cast(int)(typ));
        parameters.writeTo(writer);
        writer.write(magazine);
        writer.write(wasShooting);
        writer.write(spread);
        if (fireTimer.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(fireTimer.get);
        }
        if (lastAngle.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(lastAngle.get);
        }
        if (lastFireTick.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(lastFireTick.get);
        }
    }
    string toString() const {
        return "Weapon" ~ "(" ~
            to!string(typ) ~
            to!string(parameters) ~
            to!string(magazine) ~
            to!string(wasShooting) ~
            to!string(spread) ~
            to!string(fireTimer) ~
            to!string(lastAngle) ~
            to!string(lastFireTick) ~
            ")";
    }
}
