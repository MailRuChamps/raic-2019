package model

import util.StreamUtil

enum class TextAlignment private constructor(var discriminant: Int) {
    LEFT(0),
    CENTER(1),
    RIGHT(2)
}
