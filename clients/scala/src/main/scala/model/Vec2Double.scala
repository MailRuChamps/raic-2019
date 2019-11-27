package model

import util.StreamUtil

case class Vec2Double(x: Double = 0.0, y: Double = 0.0) {
  def writeTo(stream: java.io.OutputStream) = {
    StreamUtil.writeDouble(stream, x)
    StreamUtil.writeDouble(stream, y)
  }
}

object Vec2Double {
  def readFrom(stream: java.io.InputStream) = Vec2Double(
    StreamUtil.readDouble(stream),
    StreamUtil.readDouble(stream)
  )
}