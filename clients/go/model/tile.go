package model

import "io"
import . "aicup2019/stream"

type Tile int32
const (
    TileEmpty Tile = 0
    TileWall Tile = 1
    TilePlatform Tile = 2
    TileLadder Tile = 3
    TileJumpPad Tile = 4
)
func ReadTile(reader io.Reader) Tile {
    switch ReadInt32(reader) {
    case 0:
        return TileEmpty
    case 1:
        return TileWall
    case 2:
        return TilePlatform
    case 3:
        return TileLadder
    case 4:
        return TileJumpPad
    }
    panic("Unexpected discriminant value")
}
