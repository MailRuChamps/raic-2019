package model

import util.StreamUtil

case class ColorFloat(r: Float = 0.0f,
                      g: Float = 0.0f,
                      b: Float = 0.0f,
                      a: Float = 0.0f) {
  def writeTo(stream: java.io.OutputStream) {
    StreamUtil.writeFloat(stream, r)
    StreamUtil.writeFloat(stream, g)
    StreamUtil.writeFloat(stream, b)
    StreamUtil.writeFloat(stream, a)
  }
}

object ColorFloat {
  def readFrom(stream: java.io.InputStream): ColorFloat = {
    ColorFloat(
      StreamUtil.readFloat(stream),
      StreamUtil.readFloat(stream),
      StreamUtil.readFloat(stream),
      StreamUtil.readFloat(stream)
    )
  }
}
