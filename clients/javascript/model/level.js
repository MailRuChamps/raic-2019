const Tile = require('./tile').Tile;

class Level {
    constructor (tiles) {
        this.tiles = tiles;
    }

    static async readFrom (stream) {
        const tiles = [];
        for (let i = 0, rows = await stream.readInt(); i < rows; i++) {
            tiles[i] = [];
            for (let j = 0, cols = await stream.readInt(); j < cols; j++) {
                tiles[i][j] = await Tile.readFrom(stream);
            }
        }
        return new Level(tiles);
    }

    async writeTo (stream) {
        const rowsSize = this.tiles.length;
        await stream.writeInt(rowsSize);
        
        for (let i = 0; i < rowsSize; i++) {
            let tileRow = this.tiles[i];
            let tileRowSize = tileRow.length;
            await stream.writeInt(tileRowSize);
            for (let j = 0; j < tileRowSize; j++) {
                await stream.writeInt(tileRow[i].discriminant);
            }
        }
    }

    toString () {
        return 'Level(' +
            this.tiles +
            ')';
    }
}

module.exports.Level = Level;
