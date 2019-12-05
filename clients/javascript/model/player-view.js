const Game = require('./game').Game;
class PlayerView {
    constructor(myId, game) {
        this.myId = myId;
        this.game = game;
    }
    static async readFrom(stream) {
        let myId;
        myId = await stream.readInt();
        let game;
        game = await Game.readFrom(stream);
        return new PlayerView(myId, game);
    }
    async writeTo(stream) {
        let myId = this.myId;
        await stream.writeInt(myId);
        let game = this.game;
        await game.writeTo(stream);
    }
}
module.exports = { PlayerView: PlayerView }
