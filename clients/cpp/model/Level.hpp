#ifndef _MODEL_LEVEL_HPP_
#define _MODEL_LEVEL_HPP_

#include "../Stream.hpp"
#include <string>
#include <vector>
#include <vector>
#include <stdexcept>
#include "Tile.hpp"

class Level {
public:
    std::vector<std::vector<Tile>> tiles;
    Level();
    Level(std::vector<std::vector<Tile>> tiles);
    static Level readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
