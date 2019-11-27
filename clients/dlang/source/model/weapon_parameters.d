import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct WeaponParameters {
    int magazineSize;
    double fireRate;
    double reloadTime;
    double minSpread;
    double maxSpread;
    double recoil;
    double aimSpeed;
    BulletParameters bullet;
    Nullable!(ExplosionParameters) explosion;
    this(int magazineSize, double fireRate, double reloadTime, double minSpread, double maxSpread, double recoil, double aimSpeed, BulletParameters bullet, Nullable!(ExplosionParameters) explosion) {
        this.magazineSize = magazineSize;
        this.fireRate = fireRate;
        this.reloadTime = reloadTime;
        this.minSpread = minSpread;
        this.maxSpread = maxSpread;
        this.recoil = recoil;
        this.aimSpeed = aimSpeed;
        this.bullet = bullet;
        this.explosion = explosion;
    }
    static WeaponParameters readFrom(Stream reader) {
        auto result = WeaponParameters();
        result.magazineSize = reader.readInt();
        result.fireRate = reader.readDouble();
        result.reloadTime = reader.readDouble();
        result.minSpread = reader.readDouble();
        result.maxSpread = reader.readDouble();
        result.recoil = reader.readDouble();
        result.aimSpeed = reader.readDouble();
        result.bullet = BulletParameters.readFrom(reader);
        if (reader.readBool()) {
            result.explosion = ExplosionParameters.readFrom(reader);
        } else {
            result.explosion.nullify();
        }
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(magazineSize);
        writer.write(fireRate);
        writer.write(reloadTime);
        writer.write(minSpread);
        writer.write(maxSpread);
        writer.write(recoil);
        writer.write(aimSpeed);
        bullet.writeTo(writer);
        if (explosion.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            explosion.get.writeTo(writer);
        }
    }
    string toString() const {
        return "WeaponParameters" ~ "(" ~
            to!string(magazineSize) ~
            to!string(fireRate) ~
            to!string(reloadTime) ~
            to!string(minSpread) ~
            to!string(maxSpread) ~
            to!string(recoil) ~
            to!string(aimSpeed) ~
            to!string(bullet) ~
            to!string(explosion) ~
            ")";
    }
}
