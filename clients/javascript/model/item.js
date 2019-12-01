const WeaponType = require('./weapon-type').WeaponType;

class Item {
    static async readFrom (stream) {
        const discriminant = await stream.readInt();
        if (discriminant === HealthPack.TAG) {
            return await HealthPack.readFrom(stream);
        }
        if (discriminant === Weapon.TAG) {
            return await Weapon.readFrom(stream);
        }
        if (discriminant === Mine.TAG) {
            return await Mine.readFrom(stream);
        }
        throw new Error('Unexpected discriminant value');
    }
}

class HealthPack extends Item {
    constructor (health) {
        super();
        this.health = health;
    }

    async readFrom (stream) {
        const health = await stream.readInt();
        return new HealthPack(health);
    }

    async writeTo (stream) {
        await stream.writeInt(HealthPack.TAG);
        await stream.writeInt(this.health);
    }

    toString () {
        return 'HealthPack(' +
            this.health +
            ')';
    }
}
HealthPack.TAG = 0;

class Weapon extends Item {
    constructor (weaponType) {
        super();
        this.weaponType = weaponType;
    }
    
    static async readFrom (stream) {
        const weaponType = await WeaponType.readFrom(stream);
        return new Weapon(weaponType);
    }

    async writeTo (stream) {
        await stream.writeInt(this.TAG);
        await stream.writeInt(this.weaponType.discriminant);
    }

    toString () {
        return 'Weapon(' +
            this.weaponType +
            ')';
    }
}
Weapon.TAG = 1;

class Mine extends Item {
    static async readFrom (stream) {
        return new Mine();
    }

    async writeTo (stream) {
        await stream.writeInt(this.TAG);
    }

    toString () {
        return 'Mine(' +
            ')';
    }
}
Mine.TAG = 2;


module.exports = {
    Item: Item,
    HealthPack: HealthPack,
    Weapon: Weapon,
    Mine: Mine,
};
