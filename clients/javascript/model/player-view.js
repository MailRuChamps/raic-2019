const Game = require('./game').Game;

class PlayerView {
    constructor (myId, game) {
        this.myId = myId;
        this.game = game;
    }

    static async readFrom (stream) {
        const myId = await stream.readInt();
        const game = await Game.readFrom(stream);
        return new PlayerView(myId, game);
    }

    async writeTo (stream) {
        await stream.writeInt(this.myId);
        await this.game.writeTo(stream);
    }

    toString () {
        return 'PlayerView(' + this.myId + ',' + this.game + ')';
    }
}

module.exports.PlayerView = PlayerView;
