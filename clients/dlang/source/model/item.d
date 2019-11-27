import model;
import stream;
import std.conv;
import std.typecons : Nullable;

abstract class Item {
    abstract void writeTo(Stream writer) const;
    static Item readFrom(Stream reader) {
        switch (reader.readInt()) {
            case HealthPack.TAG:
                return HealthPack.readFrom(reader);
            case Weapon.TAG:
                return Weapon.readFrom(reader);
            case Mine.TAG:
                return Mine.readFrom(reader);
            default:
                throw new Exception("Unexpected discriminant value");
        }
    }

    static class HealthPack : Item {
        static const int TAG = 0;
        int health;
        this() {}
        this(int health) {
            this.health = health;
        }
        static HealthPack readFrom(Stream reader) {
            auto result = new HealthPack();
            result.health = reader.readInt();
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(health);
        }
        override string toString() const {
            return "HealthPack" ~ "(" ~
                to!string(health) ~
                ")";
        }
    }

    static class Weapon : Item {
        static const int TAG = 1;
        WeaponType weaponType;
        this() {}
        this(WeaponType weaponType) {
            this.weaponType = weaponType;
        }
        static Weapon readFrom(Stream reader) {
            auto result = new Weapon();
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
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(cast(int)(weaponType));
        }
        override string toString() const {
            return "Weapon" ~ "(" ~
                to!string(weaponType) ~
                ")";
        }
    }

    static class Mine : Item {
        static const int TAG = 2;
        this() {}
        static Mine readFrom(Stream reader) {
            auto result = new Mine();
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
        }
        override string toString() const {
            return "Mine" ~ "(" ~
                ")";
        }
    }
}
