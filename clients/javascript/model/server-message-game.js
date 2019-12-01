const PlayerView = require('./player-view').PlayerView;

class ServerMessageGame {
    constructor (playerView) {
        this.playerView = playerView;
    }

    static async readFrom (stream) {
        let playerView;
        if (await stream.readBool()) {
            playerView = await PlayerView.readFrom(stream);
        } else {
            playerView = null;
        }
        return new ServerMessageGame(playerView);
    }

    async writeTo (stream) {
        if (this.playerView === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await this.playerView.writeTo(stream);
        }
    }

    toString () {
        return 'ServerMessageGame(' + this.playerView + ')';
    }
}

module.exports.ServerMessageGame = ServerMessageGame;
