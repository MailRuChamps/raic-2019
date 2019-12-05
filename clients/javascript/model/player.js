class Player {
    constructor(id, score) {
        this.id = id;
        this.score = score;
    }
    static async readFrom(stream) {
        let id;
        id = await stream.readInt();
        let score;
        score = await stream.readInt();
        return new Player(id, score);
    }
    async writeTo(stream) {
        let id = this.id;
        await stream.writeInt(id);
        let score = this.score;
        await stream.writeInt(score);
    }
}
module.exports = { Player: Player }
