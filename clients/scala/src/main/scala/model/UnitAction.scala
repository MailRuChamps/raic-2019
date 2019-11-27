package model

import util.StreamUtil

case class UnitAction(velocity: Double = 0.0,
                      jump: Boolean = false,
                      jumpDown: Boolean = false,
                      aim: model.Vec2Double = Vec2Double(),
                      shoot: Boolean = false,
                      swapWeapon: Boolean = false,
                      plantMine: Boolean = false) {
  def writeTo(stream: java.io.OutputStream) = {
    StreamUtil.writeDouble(stream, velocity)
    StreamUtil.writeBoolean(stream, jump)
    StreamUtil.writeBoolean(stream, jumpDown)
    aim.writeTo(stream)
    StreamUtil.writeBoolean(stream, shoot)
    StreamUtil.writeBoolean(stream, swapWeapon)
    StreamUtil.writeBoolean(stream, plantMine)
  }
}

object UnitAction {
  def readFrom(stream: java.io.InputStream) = this (
    StreamUtil.readDouble(stream),
    StreamUtil.readBoolean(stream),
    StreamUtil.readBoolean(stream),
    Vec2Double.readFrom(stream),
    StreamUtil.readBoolean(stream),
    StreamUtil.readBoolean(stream),
    StreamUtil.readBoolean(stream)
  )

}
