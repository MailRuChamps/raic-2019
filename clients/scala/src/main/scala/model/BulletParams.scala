package model

import util.StreamUtil

case class BulletParams(speed: Double = 0.0, size: Double = 0.0, damage: Int = 0) {
  def writeTo(stream: java.io.OutputStream) {
    StreamUtil.writeDouble(stream, speed)
    StreamUtil.writeDouble(stream, size)
    StreamUtil.writeInt(stream, damage)
  }
}

object BulletParams {
  def readFrom(stream: java.io.InputStream): BulletParams = {
    BulletParams(
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readInt(stream)
    )
  }
}
