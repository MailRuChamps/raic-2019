package model

import util.StreamUtil

enum class Tile private constructor(var discriminant: Int) {
    EMPTY(0),
    WALL(1),
    PLATFORM(2),
    LADDER(3),
    JUMP_PAD(4)
}
