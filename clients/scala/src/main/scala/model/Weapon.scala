package model

import util.StreamUtil

case class Weapon(typ: model.WeaponType = WeaponType.PISTOL,
                  params: model.WeaponParams = WeaponParams(),
                  magazine: Int = 0,
                  wasShooting: Boolean = false,
                  spread: Double = 0.0,
                  fireTimer: Option[Double] = None,
                  lastAngle: Option[Double] = None,
                  lastFireTick: Option[Int] = None) {

  def writeTo(stream: java.io.OutputStream) {
    StreamUtil.writeInt(stream, typ.discriminant)
    params.writeTo(stream)
    StreamUtil.writeInt(stream, magazine)
    StreamUtil.writeBoolean(stream, wasShooting)
    StreamUtil.writeDouble(stream, spread)
    fireTimer match {
      case Some(f) =>
        StreamUtil.writeBoolean(stream, true)
        StreamUtil.writeDouble(stream, f)
      case None =>
        StreamUtil.writeBoolean(stream, false)
    }
    lastAngle match {
      case Some(a) =>
        StreamUtil.writeBoolean(stream, true)
        StreamUtil.writeDouble(stream, a)
      case None =>
        StreamUtil.writeBoolean(stream, false)
    }
    lastFireTick match {
      case Some(ft) =>
        StreamUtil.writeBoolean(stream, true)
        StreamUtil.writeInt(stream, ft)
      case None =>
        StreamUtil.writeBoolean(stream, false)
    }
  }
}

object Weapon {
  def readFrom(stream: java.io.InputStream): Weapon = Weapon(
    WeaponType.readFrom(stream),
    model.WeaponParams.readFrom(stream),
    StreamUtil.readInt(stream),
    StreamUtil.readBoolean(stream),
    StreamUtil.readDouble(stream),
    if (StreamUtil.readBoolean(stream)) Some(StreamUtil.readDouble(stream)) else None,
    if (StreamUtil.readBoolean(stream)) Some(StreamUtil.readDouble(stream)) else None,
    if (StreamUtil.readBoolean(stream)) Some(StreamUtil.readInt(stream)) else None)
}
