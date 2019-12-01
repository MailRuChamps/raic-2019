class JumpState {
    constructor (canJump, speed, maxTime, canCancel) {
        this.canJump = canJump;
        this.speed = speed;
        this.maxTime = maxTime;
        this.canCancel = canCancel;
    }
    
    static async readFrom (stream) {
        const canJump = await stream.readBool();
        const speed = await stream.readDouble();
        const maxTime = await stream.readDouble();
        const canCancel = await stream.readBool();
        return new JumpState(canJump, speed, maxTime, canCancel);
    }

    async writeTo (stream) {
        await stream.writeBool(this.canJump);
        await stream.writeDouble(this.speed);
        await stream.writeDouble(this.maxTime);
        await stream.writeBool(this.canCancel);
    }

    toString () {
        return 'JumpState(' +
            this.canJump + ',' +
            this.speed + ',' +
            this.maxTime + ',' +
            this.canCancel +
            ')';
    }
}

module.exports.JumpState = JumpState;
