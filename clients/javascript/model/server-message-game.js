const PlayerView = require('./player-view').PlayerView;
class ServerMessageGame {
    constructor(playerView) {
        this.playerView = playerView;
    }
    static async readFrom(stream) {
        let playerView;
        if (await stream.readBool()) {
            playerView = await PlayerView.readFrom(stream);
        } else {
            playerView = null;
        }
        return new ServerMessageGame(playerView);
    }
    async writeTo(stream) {
        let playerView = this.playerView;
        if (playerView === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await playerView.writeTo(stream);
        }
    }
}
module.exports = { ServerMessageGame: ServerMessageGame }
