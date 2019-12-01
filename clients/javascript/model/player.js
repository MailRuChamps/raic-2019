class Player {
    constructor (id, score) {
        this.id = id;
        this.score = score;
    }
    
    static async readFrom (stream) {
        const id = await stream.readInt();
        const score = await stream.readInt();
        return new Player(id, score);
    }

    async writeTo (stream) {
        await stream.writeInt(this.id);
        await stream.writeInt(this.score);
    }

    toString () {
        return 'Player(' +
            this.id + ',' +
            this.score +
            ')';
    }
}

module.exports.Player = Player;
