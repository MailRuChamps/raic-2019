class Item {
    static async readFrom(stream) {
        let discriminant = await stream.readInt();
        if (discriminant == HealthPack.TAG) {
            return await HealthPack.readFrom(stream);
        }
        if (discriminant == Weapon.TAG) {
            return await Weapon.readFrom(stream);
        }
        if (discriminant == Mine.TAG) {
            return await Mine.readFrom(stream);
        }
        throw new Error("Unexpected discriminant value");
    }
}

class HealthPack extends Item {
    constructor(health) {
        super();
        this.health = health;
    }
    static async readFrom(stream) {
        let health;
        health = await stream.readInt();
        return new HealthPack(health);
    }
    async writeTo(stream) {
        await stream.writeInt(HealthPack.TAG);
        let health = this.health;
        await stream.writeInt(health);
    }
}
Item.HealthPack = HealthPack;
HealthPack.TAG = 0;
class Weapon extends Item {
    constructor(weaponType) {
        super();
        this.weaponType = weaponType;
    }
    static async readFrom(stream) {
        let weaponType;
        weaponType = await stream.readInt();
        return new Weapon(weaponType);
    }
    async writeTo(stream) {
        await stream.writeInt(Weapon.TAG);
        let weaponType = this.weaponType;
        await stream.writeInt(weaponType);
    }
}
Item.Weapon = Weapon;
Weapon.TAG = 1;
class Mine extends Item {
    constructor() {
        super();
		this.Mine=1;//He Hy kak TO HADO y3HaTb 4TO ETO MuHA
    }
    static async readFrom(stream) {
        return new Mine();
    }
    async writeTo(stream) {
        await stream.writeInt(Mine.TAG);
    }
}
Item.Mine = Mine;
Mine.TAG = 2;
module.exports = { Item: Item }
