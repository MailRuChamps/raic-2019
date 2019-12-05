const Vec2Double = require('./vec2-double').Vec2Double;
class UnitAction {
    constructor(velocity, jump, jumpDown, aim, shoot, reload, swapWeapon, plantMine) {
        this.velocity = velocity;
        this.jump = jump;
        this.jumpDown = jumpDown;
        this.aim = aim;
        this.shoot = shoot;
        this.reload = reload;
        this.swapWeapon = swapWeapon;
        this.plantMine = plantMine;
    }
    static async readFrom(stream) {
        let velocity;
        velocity = await stream.readDouble();
        let jump;
        jump = await stream.readBool();
        let jumpDown;
        jumpDown = await stream.readBool();
        let aim;
        aim = await Vec2Double.readFrom(stream);
        let shoot;
        shoot = await stream.readBool();
        let reload;
        reload = await stream.readBool();
        let swapWeapon;
        swapWeapon = await stream.readBool();
        let plantMine;
        plantMine = await stream.readBool();
        return new UnitAction(velocity, jump, jumpDown, aim, shoot, reload, swapWeapon, plantMine);
    }
    async writeTo(stream) {
        let velocity = this.velocity;
        await stream.writeDouble(velocity);
        let jump = this.jump;
        await stream.writeBool(jump);
        let jumpDown = this.jumpDown;
        await stream.writeBool(jumpDown);
        let aim = this.aim;
        await aim.writeTo(stream);
        let shoot = this.shoot;
        await stream.writeBool(shoot);
        let reload = this.reload;
        await stream.writeBool(reload);
        let swapWeapon = this.swapWeapon;
        await stream.writeBool(swapWeapon);
        let plantMine = this.plantMine;
        await stream.writeBool(plantMine);
    }
}
module.exports = { UnitAction: UnitAction }
