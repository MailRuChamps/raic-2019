package model

import util.StreamUtil

case class ExplosionParams(radius: Double, damage: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeDouble(stream, radius)
        StreamUtil.writeInt(stream, damage)
    }
}
object ExplosionParams {
    def readFrom(stream: java.io.InputStream): ExplosionParams = ExplosionParams(
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readInt(stream)
        )
}
