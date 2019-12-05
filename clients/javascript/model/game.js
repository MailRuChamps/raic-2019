const Properties = require('./properties').Properties;
const Level = require('./level').Level;
const Player = require('./player').Player;
const Unit = require('./unit').Unit;
const Bullet = require('./bullet').Bullet;
const Mine = require('./mine').Mine;
const LootBox = require('./loot-box').LootBox;
class Game {
    constructor(currentTick, properties, level, players, units, bullets, mines, lootBoxes) {
        this.currentTick = currentTick;
        this.properties = properties;
        this.level = level;
        this.players = players;
        this.units = units;
        this.bullets = bullets;
        this.mines = mines;
        this.lootBoxes = lootBoxes;
    }
    static async readFrom(stream) {
        let currentTick;
        currentTick = await stream.readInt();
        let properties;
        properties = await Properties.readFrom(stream);
        let level;
        level = await Level.readFrom(stream);
        let players;
        players = [];
        for (let i = await stream.readInt(); i > 0; i--) {
            let playersElement;
            playersElement = await Player.readFrom(stream);
            players.push(playersElement);
        }
        let units;
        units = [];
        for (let i = await stream.readInt(); i > 0; i--) {
            let unitsElement;
            unitsElement = await Unit.readFrom(stream);
            units.push(unitsElement);
        }
        let bullets;
        bullets = [];
        for (let i = await stream.readInt(); i > 0; i--) {
            let bulletsElement;
            bulletsElement = await Bullet.readFrom(stream);
            bullets.push(bulletsElement);
        }
        let mines;
        mines = [];
        for (let i = await stream.readInt(); i > 0; i--) {
            let minesElement;
            minesElement = await Mine.readFrom(stream);
            mines.push(minesElement);
        }
        let lootBoxes;
        lootBoxes = [];
        for (let i = await stream.readInt(); i > 0; i--) {
            let lootBoxesElement;
            lootBoxesElement = await LootBox.readFrom(stream);
            lootBoxes.push(lootBoxesElement);
        }
        return new Game(currentTick, properties, level, players, units, bullets, mines, lootBoxes);
    }
    async writeTo(stream) {
        let currentTick = this.currentTick;
        await stream.writeInt(currentTick);
        let properties = this.properties;
        await properties.writeTo(stream);
        let level = this.level;
        await level.writeTo(stream);
        let players = this.players;
        await stream.writeInt(players.length);
        for (let playersElement of players) {
            await playersElement.writeTo(stream);
        }
        let units = this.units;
        await stream.writeInt(units.length);
        for (let unitsElement of units) {
            await unitsElement.writeTo(stream);
        }
        let bullets = this.bullets;
        await stream.writeInt(bullets.length);
        for (let bulletsElement of bullets) {
            await bulletsElement.writeTo(stream);
        }
        let mines = this.mines;
        await stream.writeInt(mines.length);
        for (let minesElement of mines) {
            await minesElement.writeTo(stream);
        }
        let lootBoxes = this.lootBoxes;
        await stream.writeInt(lootBoxes.length);
        for (let lootBoxesElement of lootBoxes) {
            await lootBoxesElement.writeTo(stream);
        }
    }
}
module.exports = { Game: Game }
