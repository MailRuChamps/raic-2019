const Vec2Double = require('./vec2-double').Vec2Double;
const ExplosionParams = require('./explosion-params').ExplosionParams;
class Mine {
    constructor(playerId, position, size, state, timer, triggerRadius, explosionParams) {
        this.playerId = playerId;
        this.position = position;
        this.size = size;
        this.state = state;
        this.timer = timer;
        this.triggerRadius = triggerRadius;
        this.explosionParams = explosionParams;
    }
    static async readFrom(stream) {
        let playerId;
        playerId = await stream.readInt();
        let position;
        position = await Vec2Double.readFrom(stream);
        let size;
        size = await Vec2Double.readFrom(stream);
        let state;
        state = stream.readInt();
        let timer;
        if (await stream.readBool()) {
            timer = await stream.readDouble();
        } else {
            timer = null;
        }
        let triggerRadius;
        triggerRadius = await stream.readDouble();
        let explosionParams;
        explosionParams = await ExplosionParams.readFrom(stream);
        return new Mine(playerId, position, size, state, timer, triggerRadius, explosionParams);
    }
    async writeTo(stream) {
        let playerId = this.playerId;
        await stream.writeInt(playerId);
        let position = this.position;
        await position.writeTo(stream);
        let size = this.size;
        await size.writeTo(stream);
        let state = this.state;
        await stream.writeInt(state);
        let timer = this.timer;
        if (timer === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeDouble(timer);
        }
        let triggerRadius = this.triggerRadius;
        await stream.writeDouble(triggerRadius);
        let explosionParams = this.explosionParams;
        await explosionParams.writeTo(stream);
    }
}
module.exports = { Mine: Mine }
