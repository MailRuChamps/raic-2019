import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Level {
    Tile[][] tiles;
    this(Tile[][] tiles) {
        this.tiles = tiles;
    }
    static Level readFrom(Stream reader) {
        auto result = Level();
        result.tiles = new Tile[][reader.readInt()];
        for (int i = 0; i < result.tiles.length; i++) {
            result.tiles[i] = new Tile[reader.readInt()];
            for (int j = 0; j < result.tiles[i].length; j++) {
                switch (reader.readInt()) {
                case 0:
                    result.tiles[i][j] = Tile.Empty;
                    break;
                case 1:
                    result.tiles[i][j] = Tile.Wall;
                    break;
                case 2:
                    result.tiles[i][j] = Tile.Platform;
                    break;
                case 3:
                    result.tiles[i][j] = Tile.Ladder;
                    break;
                case 4:
                    result.tiles[i][j] = Tile.JumpPad;
                    break;
                default:
                    throw new Exception("Unexpected discriminant value");
                }
            }
        }
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(cast(int)(tiles.length));
        foreach (tilesElement; tiles) {
            writer.write(cast(int)(tilesElement.length));
            foreach (tilesElementElement; tilesElement) {
                writer.write(cast(int)(tilesElementElement));
            }
        }
    }
    string toString() const {
        return "Level" ~ "(" ~
            to!string(tiles) ~
            ")";
    }
}
