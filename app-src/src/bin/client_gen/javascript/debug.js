const PlayerMessageGame = require('./model/player-message-game').PlayerMessageGame;

class Debug {
    constructor (streamWrapper) {
        this.streamWrapper = streamWrapper;
    }

    async draw (data) {
        await (new PlayerMessageGame.CustomDataMessage(data)).writeTo(this.streamWrapper);
    }
}

module.exports.Debug = Debug;
