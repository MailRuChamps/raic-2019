class JumpState {
    constructor(canJump, speed, maxTime, canCancel) {
        this.canJump = canJump;
        this.speed = speed;
        this.maxTime = maxTime;
        this.canCancel = canCancel;
    }
    static async readFrom(stream) {
        let canJump;
        canJump = await stream.readBool();
        let speed;
        speed = await stream.readDouble();
        let maxTime;
        maxTime = await stream.readDouble();
        let canCancel;
        canCancel = await stream.readBool();
        return new JumpState(canJump, speed, maxTime, canCancel);
    }
    async writeTo(stream) {
        let canJump = this.canJump;
        await stream.writeBool(canJump);
        let speed = this.speed;
        await stream.writeDouble(speed);
        let maxTime = this.maxTime;
        await stream.writeDouble(maxTime);
        let canCancel = this.canCancel;
        await stream.writeBool(canCancel);
    }
}
module.exports = { JumpState: JumpState }
