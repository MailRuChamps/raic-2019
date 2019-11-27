package model

import util.StreamUtil

enum class MineState private constructor(var discriminant: Int) {
    PREPARING(0),
    IDLE(1),
    TRIGGERED(2),
    EXPLODED(3)
}
