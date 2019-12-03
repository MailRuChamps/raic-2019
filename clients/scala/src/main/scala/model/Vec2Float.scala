package model

import util.StreamUtil

case class Vec2Float(x: Float, y: Float) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeFloat(stream, x)
        StreamUtil.writeFloat(stream, y)
    }
}
object Vec2Float {
    def readFrom(stream: java.io.InputStream): Vec2Float = Vec2Float(
        StreamUtil.readFloat(stream)
        ,
        StreamUtil.readFloat(stream)
        )
}
