package model

import util.StreamUtil

case class Versioned(inner: Map[Int, model.UnitAction]) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, 43981)
        StreamUtil.writeInt(stream, inner.size)
        inner.foreach { case (key, value) =>
            StreamUtil.writeInt(stream, key)
            value.writeTo(stream)
        }
    }
}
object Versioned {
    def readFrom(stream: java.io.InputStream): Versioned = Versioned(
        (0 until StreamUtil.readInt(stream)).map { _ => (
            StreamUtil.readInt(stream)
            ,
            model.UnitAction.readFrom(stream)
        )}.toMap
        )
}
