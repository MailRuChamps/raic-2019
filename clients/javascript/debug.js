const CustomDataMessage = require('./model/player-message-game').CustomDataMessage;

class Debug {
    constructor (streamWrapper) {
        this.streamWrapper = streamWrapper;
    }

    async draw (data) {
        await CustomDataMessage(data).writeTo(this.streamWrapper);
    }
}

module.exports.Debug = Debug;
