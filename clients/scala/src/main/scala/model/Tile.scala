package model

import util.StreamUtil

sealed abstract class Tile (val discriminant: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, discriminant)
    }
}

object Tile {
    case object EMPTY extends Tile(0)
    case object WALL extends Tile(1)
    case object PLATFORM extends Tile(2)
    case object LADDER extends Tile(3)
    case object JUMP_PAD extends Tile(4)
    def readFrom(stream: java.io.InputStream): Tile = StreamUtil.readInt(stream) match {
        case 0 => EMPTY
        case 1 => WALL
        case 2 => PLATFORM
        case 3 => LADDER
        case 4 => JUMP_PAD
        case _ => throw new java.io.IOException("Unexpected discriminant value")
    }
}
