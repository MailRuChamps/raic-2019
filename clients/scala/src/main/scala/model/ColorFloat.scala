package model

import util.StreamUtil

case class ColorFloat(r: Float, g: Float, b: Float, a: Float) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeFloat(stream, r)
        StreamUtil.writeFloat(stream, g)
        StreamUtil.writeFloat(stream, b)
        StreamUtil.writeFloat(stream, a)
    }
}
object ColorFloat {
    def readFrom(stream: java.io.InputStream): ColorFloat = ColorFloat(
        StreamUtil.readFloat(stream)
        ,
        StreamUtil.readFloat(stream)
        ,
        StreamUtil.readFloat(stream)
        ,
        StreamUtil.readFloat(stream)
        )
}
