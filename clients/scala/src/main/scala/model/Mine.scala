package model

import util.StreamUtil

case class Mine(playerId: Int, position: model.Vec2Double, size: model.Vec2Double, state: model.MineState, timer: Option[Double], triggerRadius: Double, explosionParams: model.ExplosionParams) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, playerId)
        position.writeTo(stream)
        size.writeTo(stream)
        state.writeTo(stream)
        timer match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                StreamUtil.writeDouble(stream, value)
            }
        }
        StreamUtil.writeDouble(stream, triggerRadius)
        explosionParams.writeTo(stream)
    }
}
object Mine {
    def readFrom(stream: java.io.InputStream): Mine = Mine(
        StreamUtil.readInt(stream)
        ,
        model.Vec2Double.readFrom(stream)
        ,
        model.Vec2Double.readFrom(stream)
        ,
        model.MineState.readFrom(stream)
        ,
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readDouble(stream)
        ) else None
        ,
        StreamUtil.readDouble(stream)
        ,
        model.ExplosionParams.readFrom(stream)
        )
}
