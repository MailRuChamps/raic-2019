const Vec2Double = require('./vec2-double').Vec2Double;
const JumpState = require('./jump-state').JumpState;
const Weapon = require('./weapon').Weapon;
class Unit {
    constructor(playerId, id, health, position, size, jumpState, walkedRight, stand, onGround, onLadder, mines, weapon) {
        this.playerId = playerId;
        this.id = id;
        this.health = health;
        this.position = position;
        this.size = size;
        this.jumpState = jumpState;
        this.walkedRight = walkedRight;
        this.stand = stand;
        this.onGround = onGround;
        this.onLadder = onLadder;
        this.mines = mines;
        this.weapon = weapon;
    }
    static async readFrom(stream) {
        let playerId;
        playerId = await stream.readInt();
        let id;
        id = await stream.readInt();
        let health;
        health = await stream.readInt();
        let position;
        position = await Vec2Double.readFrom(stream);
        let size;
        size = await Vec2Double.readFrom(stream);
        let jumpState;
        jumpState = await JumpState.readFrom(stream);
        let walkedRight;
        walkedRight = await stream.readBool();
        let stand;
        stand = await stream.readBool();
        let onGround;
        onGround = await stream.readBool();
        let onLadder;
        onLadder = await stream.readBool();
        let mines;
        mines = await stream.readInt();
        let weapon;
        if (await stream.readBool()) {
            weapon = await Weapon.readFrom(stream);
        } else {
            weapon = null;
        }
        return new Unit(playerId, id, health, position, size, jumpState, walkedRight, stand, onGround, onLadder, mines, weapon);
    }
    async writeTo(stream) {
        let playerId = this.playerId;
        await stream.writeInt(playerId);
        let id = this.id;
        await stream.writeInt(id);
        let health = this.health;
        await stream.writeInt(health);
        let position = this.position;
        await position.writeTo(stream);
        let size = this.size;
        await size.writeTo(stream);
        let jumpState = this.jumpState;
        await jumpState.writeTo(stream);
        let walkedRight = this.walkedRight;
        await stream.writeBool(walkedRight);
        let stand = this.stand;
        await stream.writeBool(stand);
        let onGround = this.onGround;
        await stream.writeBool(onGround);
        let onLadder = this.onLadder;
        await stream.writeBool(onLadder);
        let mines = this.mines;
        await stream.writeInt(mines);
        let weapon = this.weapon;
        if (weapon === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await weapon.writeTo(stream);
        }
    }
}
module.exports = { Unit: Unit }
