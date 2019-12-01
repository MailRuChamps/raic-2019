const Level = require('./level').Level;
const Properties = require('./properties').Properties;
const Player = require('./player').Player;
const Unit = require('./unit').Unit;
const Bullet = require('./bullet').Bullet;
const Mine = require('./mine').Mine;
const LootBox = require('./loot-box').LootBox;

class Game {
    constructor (currentTick, properties, level, players, units, bullets, mines, lootBoxes) {
        this.currentTick = currentTick;
        this.properties = properties;
        this.level = level;
        this.players = players;
        this.units = units;
        this.bullets = bullets;
        this.mines = mines;
        this.lootBoxes = lootBoxes;
    }

    static async readFrom (stream) {
        const currentTick = await stream.readInt();
        const properties = await Properties.readFrom(stream);
        const level = await Level.readFrom(stream);

        let i, arraySize;
        const players = [];
        for (i = 0, arraySize = await stream.readInt(); i < arraySize; i++) {
            players.push(await Player.readFrom(stream));
        }

        const units = [];
        for (i = 0, arraySize = await stream.readInt(); i < arraySize; i++) {
            units.push(await Unit.readFrom(stream));
        }

        const bullets = [];
        for (i = 0, arraySize = await stream.readInt(); i < arraySize; i++) {
            bullets.push(await Bullet.readFrom(stream));
        }

        const mines = [];
        for (i = 0, arraySize = await stream.readInt(); i < arraySize; i++) {
            mines.push(await Mine.readFrom(stream));
        }

        const lootBoxes = [];
        for (i = 0, arraySize = await stream.readInt(); i < arraySize; i++) {
            lootBoxes.push(await LootBox.readFrom(stream));
        }

        return new Game(currentTick, properties, level, players, units, bullets, mines, lootBoxes);
    }

    async writeTo (stream) {
        await stream.writeInt(this.currentTick);
        await this.properties.writeTo(stream);
        await this.level.writeTo(stream);

        const playersSize = this.players.length;
        await stream.writeInt(playersSize);
        for (let i = 0; i < playersSize; i++) {
            await this.players[i].writeTo(stream);
        }

        const unitsSize = this.units.length;
        await stream.writeInt(unitsSize);
        for (let i = 0; i < unitsSize; i++) {
            await this.units[i].writeTo(stream);
        }

        const bulletsSize = this.bullets.length;
        await stream.writeInt(bulletsSize);
        for (let i = 0; i < bulletsSize; i++) {
            await this.bullets[i].writeTo(stream);
        }

        const minesSize = this.mines.length;
        await stream.writeInt(minesSize);
        for (let i = 0; i < minesSize; i++) {
            await this.mines[i].writeTo(stream);
        }

        const lootBoxesSize = this.lootBoxes.length;
        await stream.writeInt(lootBoxesSize);
        for (let i = 0; i < lootBoxesSize; i++) {
            await this.lootBoxes[i].writeTo(stream);
        }
    }

    toString () {
        return 'Game(' +
            this.currentTick + ',' +
            this.properties + ',' +
            this.level + ',' +
            this.players + ',' +
            this.units + ',' +
            this.bullets + ',' +
            this.mines + ',' +
            this.lootBoxes +
            ')';
    }
}

module.exports.Game = Game;
