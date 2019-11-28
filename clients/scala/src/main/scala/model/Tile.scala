package model

import util.StreamUtil

sealed abstract class Tile(val discriminant: Int)

object Tile {
  case object EMPTY extends Tile(0)
  case object WALL extends Tile(1)
  case object PLATFORM extends Tile(2)
  case object LADDER extends Tile(3)
  case object JUMP_PAD extends Tile(4)

  def readFrom(stream: java.io.InputStream): Tile = StreamUtil.readInt(stream) match {
    case 0 => model.Tile.EMPTY
    case 1 => model.Tile.WALL
    case 2 => model.Tile.PLATFORM
    case 3 => model.Tile.LADDER
    case 4 => model.Tile.JUMP_PAD
    case _ => throw new java.io.IOException("Unexpected discriminant value")
  }
}
