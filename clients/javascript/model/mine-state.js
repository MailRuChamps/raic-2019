class MineState {
    constructor (discriminant) {
        this.discriminant = discriminant;
    }

    static async readFrom (stream) {
        switch (await stream.readInt()) {
        case 0:
            return new MineState(MineState.PREPARING);
        case 1:
            return new MineState(MineState.IDLE);
        case 2:
            return new MineState(MineState.TRIGGERED);
        case 3:
            return new MineState(MineState.EXPLODED);
        default:
            throw new Error('Unexpected discriminant value');
        }
    }
}

MineState.PREPARING = 0;
MineState.IDLE = 1;
MineState.TRIGGERED = 2;
MineState.EXPLODED = 3;

module.exports.MineState = MineState;
