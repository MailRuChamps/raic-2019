package model;

import util.StreamUtil;

public abstract class Item {
    public abstract void writeTo(java.io.OutputStream stream) throws java.io.IOException;
    public static Item readFrom(java.io.InputStream stream) throws java.io.IOException {
        switch (StreamUtil.readInt(stream)) {
            case HealthPack.TAG:
                return HealthPack.readFrom(stream);
            case Weapon.TAG:
                return Weapon.readFrom(stream);
            case Mine.TAG:
                return Mine.readFrom(stream);
            default:
                throw new java.io.IOException("Unexpected discriminant value");
        }
    }

    public static class HealthPack extends Item {
        public static final int TAG = 0;
        private int health;
        public int getHealth() { return health; }
        public void setHealth(int health) { this.health = health; }
        public HealthPack() {}
        public HealthPack(int health) {
            this.health = health;
        }
        public static HealthPack readFrom(java.io.InputStream stream) throws java.io.IOException {
            HealthPack result = new HealthPack();
            result.health = StreamUtil.readInt(stream);
            return result;
        }
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, health);
        }
    }

    public static class Weapon extends Item {
        public static final int TAG = 1;
        private model.WeaponType weaponType;
        public model.WeaponType getWeaponType() { return weaponType; }
        public void setWeaponType(model.WeaponType weaponType) { this.weaponType = weaponType; }
        public Weapon() {}
        public Weapon(model.WeaponType weaponType) {
            this.weaponType = weaponType;
        }
        public static Weapon readFrom(java.io.InputStream stream) throws java.io.IOException {
            Weapon result = new Weapon();
            switch (StreamUtil.readInt(stream)) {
            case 0:
                result.weaponType = model.WeaponType.PISTOL;
                break;
            case 1:
                result.weaponType = model.WeaponType.ASSAULT_RIFLE;
                break;
            case 2:
                result.weaponType = model.WeaponType.ROCKET_LAUNCHER;
                break;
            default:
                throw new java.io.IOException("Unexpected discriminant value");
            }
            return result;
        }
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, weaponType.discriminant);
        }
    }

    public static class Mine extends Item {
        public static final int TAG = 2;
        public Mine() {}
        public static Mine readFrom(java.io.InputStream stream) throws java.io.IOException {
            Mine result = new Mine();
            return result;
        }
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
        }
    }
}
