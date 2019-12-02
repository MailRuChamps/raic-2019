'use strict';

const StreamWrapper = require('./stream-wrapper').StreamWrapper;
const Socket = require('net').Socket;
const ServerMessageGame = require('./model/server-message-game').ServerMessageGame;
const ActionMessage = require('./model/player-message-game').ActionMessage;
const MyStrategy = require('./my-strategy').MyStrategy;
const Debug = require('./debug').Debug;

class Runner {
    constructor (host, port, token) {
        this.socket = new Socket({readable: true, writable: true});
        this.socket
            .setNoDelay(true)
            .on('error', (error) => {
                console.error('Socket error: ' + error.message);
                process.exit(1);
            });
        this.streamWrapper = new StreamWrapper(this.socket);
        this.host = host;
        this.port = port;
        this.token = token;
    }

    async connect () {
        this.socket.connect({
            host: this.host,
            port: this.port
        });
        await this.streamWrapper.writeString(this.token);
    }

    async run () {
        try {
            await this.connect();
            let message, playerView, actions;
            const strategy = new MyStrategy();
            const debug = new Debug(this.streamWrapper);
            while (true) {
                message = await ServerMessageGame.readFrom(this.streamWrapper);
                if (message.playerView === null) {
                    break;
                }
                playerView = message.playerView;
                actions = {};
                for (let i = 0, unitsSize = playerView.game.units.length; i < unitsSize; i++) {
                    let unit = playerView.game.units[i];
                    if (unit.playerId === playerView.myId) {
                        actions[unit.id] = await strategy.getAction(unit, playerView.game, debug);
                    }
                }
                await (new ActionMessage(actions)).writeTo(this.streamWrapper);
            }
        } catch (e) {
            console.error(e);
            process.exit(1);
        }
    }
}


const argv = process.argv;
const argc = argv.length;
const host = argc < 3 ? '127.0.0.1' : argv[2];
const port = argc < 4 ? 31001 : parseInt(argv[3]);
const token = argc < 5 ? '0000000000000000' : argv[4];
(new Runner(host, port, token)).run();
