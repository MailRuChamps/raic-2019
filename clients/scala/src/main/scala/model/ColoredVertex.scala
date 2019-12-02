package model

import util.StreamUtil

case class ColoredVertex(position: model.Vec2Float, color: model.ColorFloat) {
    def writeTo(stream: java.io.OutputStream) {
        position.writeTo(stream)
        color.writeTo(stream)
    }
}
object ColoredVertex {
    def readFrom(stream: java.io.InputStream): ColoredVertex = ColoredVertex(
        model.Vec2Float.readFrom(stream)
        ,
        model.ColorFloat.readFrom(stream)
        )
}
