package model

sealed abstract class Tile(val discriminant: Int)

object Tile {
  case object EMPTY extends Tile(0)
  case object WALL extends Tile(1)
  case object PLATFORM extends Tile(2)
  case object LADDER extends Tile(3)
  case object JUMP_PAD extends Tile(4)
}
