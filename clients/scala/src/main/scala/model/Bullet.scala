package model

import util.StreamUtil

case class Bullet(
                   weaponType: model.WeaponType = WeaponType.PISTOL,
                   unitId: Int = 0,
                   playerId: Int = 0,
                   position: model.Vec2Double = Vec2Double(),
                   velocity: model.Vec2Double = Vec2Double(),
                   damage: Int = 0,
                   size: Double = 0.0,
                   explosionParams: Option[model.ExplosionParams] = None
                 ) {
  def writeTo(stream: java.io.OutputStream) {
    StreamUtil.writeInt(stream, weaponType.discriminant)
    StreamUtil.writeInt(stream, unitId)
    StreamUtil.writeInt(stream, playerId)
    position.writeTo(stream)
    velocity.writeTo(stream)
    StreamUtil.writeInt(stream, damage)
    StreamUtil.writeDouble(stream, size)
    explosionParams match {
      case Some(p) =>
        StreamUtil.writeBoolean(stream, true)
        p.writeTo(stream)
      case None =>
        StreamUtil.writeBoolean(stream, false)
    }
  }
}

object Bullet {
  def readFrom(stream: java.io.InputStream): Bullet = {
    Bullet(
      StreamUtil.readInt(stream) match {
        case 0 => model.WeaponType.PISTOL
        case 1 => model.WeaponType.ASSAULT_RIFLE
        case 2 => model.WeaponType.ROCKET_LAUNCHER
        case _ => throw new java.io.IOException ("Unexpected discriminant value")
      },
      StreamUtil.readInt(stream),
      StreamUtil.readInt(stream),
      model.Vec2Double.readFrom(stream),
      model.Vec2Double.readFrom(stream),
      StreamUtil.readInt(stream),
      StreamUtil.readDouble(stream),
      if (StreamUtil.readBoolean(stream)) Some(model.ExplosionParams.readFrom(stream)) else None
    )
  }
}