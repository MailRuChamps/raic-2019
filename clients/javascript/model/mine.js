const Vec2Double = require('./vec2double').Vec2Double;
const MineState = require('./mine-state').MineState;
const ExplosionParams = require('./explosion-params').ExplosionParams;

class Mine {
    constructor (playerId, position, size, state, timer, triggerRadius, explosionParams) {
        this.playerId = playerId;
        this.position = position;
        this.size = size;
        this.state = state;
        this.timer = timer;
        this.triggerRadius = triggerRadius;
        this.explosionParams = explosionParams;
    }
    
    static async readFrom (stream) {
        const playerId = await stream.readInt();
        const position = await Vec2Double.readFrom(stream);
        const size = await Vec2Double.readFrom(stream);
        const state = await MineState.readFrom(stream);
        let timer;
        if (await stream.readBool()) {
            timer = await stream.readDouble();
        } else {
            timer = null;
        }
        const triggerRadius = await stream.readDouble();
        const explosionParams = await ExplosionParams.readFrom(stream);
        return new Mine(playerId, position, size, state, timer, triggerRadius, explosionParams);
    }

    async writeTo (stream) {
        await stream.writeInt(this.playerId);
        await this.position.writeTo(stream);
        await this.size.writeTo(stream);
        await stream.writeInt(this.state.discriminant);
        if (this.timer === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeDouble(this.timer);
        }
        await stream.writeDouble(this.triggerRadius);
        await this.explosionParams.writeTo(stream);
    }

    toString () {
        return 'Mine(' +
            this.playerId + ',' +
            this.position + ',' +
            this.size + ',' +
            this.state + ',' +
            this.timer + ',' +
            this.triggerRadius + ',' +
            this.explosionParams +
            ')';
    }
}

module.exports.Mine = Mine;
