package model

import util.StreamUtil

case class Mine(
                 playerId: Int = 0,
                 position: model.Vec2Double = Vec2Double(),
                 size: model.Vec2Double = Vec2Double(),
                 state: model.MineState = MineState.IDLE,
                 timer: Option[Double] = None,
                 triggerRadius: Double = 0.0,
                 explosionParams: model.ExplosionParams = ExplosionParams()
               ) {

  def writeTo(stream: java.io.OutputStream) {
    StreamUtil.writeInt(stream, playerId)
    position.writeTo(stream)
    size.writeTo(stream)
    StreamUtil.writeInt(stream, state.discriminant)
    timer match {
      case Some(t) =>
        StreamUtil.writeBoolean(stream, true)
        StreamUtil.writeDouble(stream, t)
      case _ =>
        StreamUtil.writeBoolean(stream, false)
    }
    StreamUtil.writeDouble(stream, triggerRadius)
    explosionParams.writeTo(stream)
  }
}

object Mine {

  def readFrom(stream: java.io.InputStream): Mine = Mine(
    StreamUtil.readInt(stream),
    model.Vec2Double.readFrom(stream),
    model.Vec2Double.readFrom(stream),
    StreamUtil.readInt(stream) match {
      case 0 => model.MineState.PREPARING
      case 1 => model.MineState.IDLE
      case 2 => model.MineState.TRIGGERED
      case 3 => model.MineState.EXPLODED
      case _ => throw new java.io.IOException("Unexpected discriminant value")
    },
    if (StreamUtil.readBoolean(stream)) Some(StreamUtil.readDouble(stream)) else None,
    StreamUtil.readDouble(stream),
    model.ExplosionParams.readFrom(stream))
}
