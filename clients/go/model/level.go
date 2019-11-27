package model

import "io"
import . "aicup2019/stream"

type Level struct {
    Tiles [][]Tile
}
func NewLevel(tiles [][]Tile) Level {
    return Level {
        Tiles: tiles,
    }
}
func ReadLevel(reader io.Reader) Level {
    result := Level {}
    result.Tiles = make([][]Tile, ReadInt32(reader))
    for i := range result.Tiles {
        result.Tiles[i] = make([]Tile, ReadInt32(reader))
        for j := range result.Tiles[i] {
            result.Tiles[i][j] = ReadTile(reader)
        }
    }
    return result
}
func (value Level) Write(writer io.Writer) {
    WriteInt32(writer, int32(len(value.Tiles)))
    for _, TilesElement := range value.Tiles {
        WriteInt32(writer, int32(len(TilesElement)))
        for _, TilesElementElement := range TilesElement {
            WriteInt32(writer, int32(TilesElementElement))
        }
    }
}
