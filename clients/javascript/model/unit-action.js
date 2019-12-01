const Vec2Double = require('./vec2double').Vec2Double;

class UnitAction {
    constructor (velocity, jump, jumpDown, aim, shoot, swapWeapon, plantMine) {
        this.velocity = velocity;
        this.jump = jump;
        this.jumpDown = jumpDown;
        this.aim = aim;
        this.shoot = shoot;
        this.swapWeapon = swapWeapon;
        this.plantMine = plantMine;
    }

    static async readFrom (stream) {
        const velocity = await stream.readDouble();
        const jump = await stream.readBool();
        const jumpDown = await stream.readBool();
        const aim = await Vec2Double.readFrom(stream);
        const shoot = await stream.readBool();
        const swapWeapon = await stream.readBool();
        const plantMine = await stream.readBool();
        return new UnitAction(velocity, jump, jumpDown, aim, shoot, swapWeapon, plantMine);
    }
    
    async writeTo (stream) {
        await stream.writeDouble(this.velocity);
        await stream.writeBool(this.jump);
        await stream.writeBool(this.jumpDown);
        await this.aim.writeTo(stream);
        await stream.writeBool(this.shoot);
        await stream.writeBool(this.swapWeapon);
        await stream.writeBool(this.plantMine);
    }

    toString () {
        return 'UnitAction(' +
            this.velocity + ',' +
            this.jump + ',' +
            this.jumpDown + ',' +
            this.aim + ',' +
            this.shoot + ',' +
            this.swapWeapon + ',' +
            this.plantMine +
            ')';
    }
}

module.exports.UnitAction = UnitAction;