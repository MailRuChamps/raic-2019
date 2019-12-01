const Vec2Double = require('./vec2double').Vec2Double;
const JumpState = require('./jump-state').JumpState;
const Weapon = require('./weapon').Weapon;

class Unit {
    constructor (playerId, id, health, position, size, jumpState, walkedRight, stand, onGround, onLadder, mines, weapon) {
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

    static async readFrom (stream) {
        const playerId = await stream.readInt();
        const id = await stream.readInt();
        const health = await stream.readInt();
        const position = await Vec2Double.readFrom(stream);
        const size = await Vec2Double.readFrom(stream);
        const jumpState = await JumpState.readFrom(stream);
        const walkedRight = await stream.readBool();
        const stand = await stream.readBool();
        const onGround = await stream.readBool();
        const onLadder = await stream.readBool();
        const mines = await stream.readInt();
        
        let weapon;
        if (await stream.readBool()) {
            weapon = await Weapon.readFrom(stream);
        } else {
            weapon = null;
        }

        return new Unit(playerId, id, health, position, size, jumpState, walkedRight, stand, onGround, onLadder, mines, weapon);
    }
    async writeTo (stream) {
        await stream.writeInt(this.playerId);
        await stream.writeInt(this.id);
        await stream.writeInt(this.health);
        await this.position.writeTo(stream);
        await this.size.writeTo(stream);
        await this.jumpState.writeTo(stream);
        await stream.writeBool(this.walkedRight);
        await stream.writeBool(this.stand);
        await stream.writeBool(this.onGround);
        await stream.writeBool(this.onLadder);
        await stream.writeInt(this.mines);
        if (this.weapon === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
        }
        await this.weapon.writeTo(stream);
    }

    toString () {
        return 'Unit(' +
            this.playerId + ',' +
            this.id + ',' +
            this.health + ',' +
            this.position + ',' +
            this.size + ',' +
            this.jumpState + ',' +
            this.walkedRight + ',' +
            this.stand + ',' +
            this.onGround + ',' +
            this.onLadder + ',' +
            this.mines + ',' +
            this.weapon +
            ')';
    }
}

module.exports.Unit = Unit;
