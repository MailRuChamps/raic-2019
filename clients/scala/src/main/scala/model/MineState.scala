package model


sealed abstract class MineState(val discriminant: Int)

object MineState {
  case object PREPARING extends MineState(0)
  case object IDLE extends MineState(1)
  case object TRIGGERED extends MineState(2)
  case object EXPLODED extends MineState(2)
}
