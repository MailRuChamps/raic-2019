#include "Level.hpp"

Level::Level() { }
Level::Level(std::vector<std::vector<Tile>> tiles) : tiles(tiles) { }
Level Level::readFrom(InputStream& stream) {
    Level result;
    result.tiles = std::vector<std::vector<Tile>>(stream.readInt());
    for (size_t i = 0; i < result.tiles.size(); i++) {
        result.tiles[i] = std::vector<Tile>(stream.readInt());
        for (size_t j = 0; j < result.tiles[i].size(); j++) {
            switch (stream.readInt()) {
            case 0:
                result.tiles[i][j] = Tile::EMPTY;
                break;
            case 1:
                result.tiles[i][j] = Tile::WALL;
                break;
            case 2:
                result.tiles[i][j] = Tile::PLATFORM;
                break;
            case 3:
                result.tiles[i][j] = Tile::LADDER;
                break;
            case 4:
                result.tiles[i][j] = Tile::JUMP_PAD;
                break;
            default:
                throw std::runtime_error("Unexpected discriminant value");
            }
        }
    }
    return result;
}
void Level::writeTo(OutputStream& stream) const {
    stream.write((int)(tiles.size()));
    for (const std::vector<Tile>& tilesElement : tiles) {
        stream.write((int)(tilesElement.size()));
        for (const Tile& tilesElementElement : tilesElement) {
            stream.write((int)(tilesElementElement));
        }
    }
}
std::string Level::toString() const {
    return std::string("Level") + "(" +
        "TODO" + 
        ")";
}
