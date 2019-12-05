class Level {
    constructor(tiles) {
        this.tiles = tiles;
    }
    static async readFrom(stream) {
        let tiles;
        tiles = [];
        for (let i = await stream.readInt(); i > 0; i--) {
            let tilesElement;
            tilesElement = [];
            for (let j = await stream.readInt(); j > 0; j--) {
                let tilesElementElement;
                tilesElementElement = stream.readInt();
                tilesElement.push(tilesElementElement);
            }
            tiles.push(tilesElement);
        }
        return new Level(tiles);
    }
    async writeTo(stream) {
        let tiles = this.tiles;
        await stream.writeInt(tiles.length);
        for (let tilesElement of tiles) {
            await stream.writeInt(tilesElement.length);
            for (let tilesElementElement of tilesElement) {
                await stream.writeInt(tilesElementElement);
            }
        }
    }
}
module.exports = { Level: Level }
