class WeaponType {
    constructor (discriminant) {
        this.discriminant = discriminant;
    }

    static async readFrom (stream) {
        switch (await stream.readInt()) {
        case 0:
            return new WeaponType(WeaponType.PISTOL);
        case 1:
            return new WeaponType(WeaponType.ASSAULT_RIFLE);
        case 2:
            return new WeaponType(WeaponType.ROCKET_LAUNCHER);
        default:
            throw new Error('Unexpected discriminant value');
        }
    }
}

WeaponType.PISTOL = 0;
WeaponType.ASSAULT_RIFLE = 1;
WeaponType.ROCKET_LAUNCHER = 2;

module.exports.WeaponType = WeaponType;
