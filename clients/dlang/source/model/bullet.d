import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Bullet {
    WeaponType weaponType;
    int unitId;
    int playerId;
    Vec2Double position;
    Vec2Double velocity;
    int damage;
    double size;
    Nullable!(ExplosionParameters) explosionParameters;
    this(WeaponType weaponType, int unitId, int playerId, Vec2Double position, Vec2Double velocity, int damage, double size, Nullable!(ExplosionParameters) explosionParameters) {
        this.weaponType = weaponType;
        this.unitId = unitId;
        this.playerId = playerId;
        this.position = position;
        this.velocity = velocity;
        this.damage = damage;
        this.size = size;
        this.explosionParameters = explosionParameters;
    }
    static Bullet readFrom(Stream reader) {
        auto result = Bullet();
        switch (reader.readInt()) {
        case 0:
            result.weaponType = WeaponType.Pistol;
            break;
        case 1:
            result.weaponType = WeaponType.AssaultRifle;
            break;
        case 2:
            result.weaponType = WeaponType.RocketLauncher;
            break;
        default:
            throw new Exception("Unexpected discriminant value");
        }
        result.unitId = reader.readInt();
        result.playerId = reader.readInt();
        result.position = Vec2Double.readFrom(reader);
        result.velocity = Vec2Double.readFrom(reader);
        result.damage = reader.readInt();
        result.size = reader.readDouble();
        if (reader.readBool()) {
            result.explosionParameters = ExplosionParameters.readFrom(reader);
        } else {
            result.explosionParameters.nullify();
        }
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(cast(int)(weaponType));
        writer.write(unitId);
        writer.write(playerId);
        position.writeTo(writer);
        velocity.writeTo(writer);
        writer.write(damage);
        writer.write(size);
        if (explosionParameters.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            explosionParameters.get.writeTo(writer);
        }
    }
    string toString() const {
        return "Bullet" ~ "(" ~
            to!string(weaponType) ~
            to!string(unitId) ~
            to!string(playerId) ~
            to!string(position) ~
            to!string(velocity) ~
            to!string(damage) ~
            to!string(size) ~
            to!string(explosionParameters) ~
            ")";
    }
}
