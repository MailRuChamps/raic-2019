package model

import util.StreamUtil

case class Vec2Float(x: Float = 0.0f, y: Float = 0.0f) {
  def writeTo(stream: java.io.OutputStream) = {
    StreamUtil.writeFloat(stream, x)
    StreamUtil.writeFloat(stream, y)
  }
}

object Vec2Float {
  def readFrom(stream: java.io.InputStream) = Vec2Float(
    StreamUtil.readFloat(stream),
    StreamUtil.readFloat(stream)
  )
}