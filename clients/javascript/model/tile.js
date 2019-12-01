class Tile {
    constructor (discriminant) {
        this.discriminant = discriminant;
    }

    static async readFrom (stream) {
        switch (await stream.readInt()) {
        case 0:
            return new Tile(Tile.EMPTY);
        case 1:
            return new Tile(Tile.WALL);
        case 2:
            return new Tile(Tile.PLATFORM);
        case 3:
            return new Tile(Tile.LADDER);
        case 4:
            return new Tile(Tile.JUMP_PAD);
        default:
            throw new Error('Unexpected discriminant value');
        }
    }
}

Tile.EMPTY = 0;
Tile.WALL = 1;
Tile.PLATFORM = 2;
Tile.LADDER = 3;
Tile.JUMP_PAD = 4;

module.exports.Tile = Tile;
