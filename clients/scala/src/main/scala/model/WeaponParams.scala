package model

import util.StreamUtil

case class WeaponParams(magazineSize: Int = 0,
                        fireRate: Double = 0.0,
                        reloadTime: Double = 0.0,
                        minSpread: Double = 0.0,
                        maxSpread: Double = 0.0,
                        recoil: Double = 0.0,
                        aimSpeed: Double = 0.0,
                        bullet: model.BulletParams = model.BulletParams(),
                        explosion: Option[model.ExplosionParams] = None) {
  def writeTo(stream: java.io.OutputStream) {
    StreamUtil.writeInt(stream, magazineSize)
    StreamUtil.writeDouble(stream, fireRate)
    StreamUtil.writeDouble(stream, reloadTime)
    StreamUtil.writeDouble(stream, minSpread)
    StreamUtil.writeDouble(stream, maxSpread)
    StreamUtil.writeDouble(stream, recoil)
    StreamUtil.writeDouble(stream, aimSpeed)
    bullet.writeTo(stream)
    explosion match {
      case Some(e) =>
        StreamUtil.writeBoolean(stream, value = true)
        e.writeTo(stream)
      case None =>
        StreamUtil.writeBoolean(stream, value = false)
    }
  }
}

object WeaponParams {
  def readFrom(stream: java.io.InputStream): WeaponParams = {
    WeaponParams(
      StreamUtil.readInt(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      model.BulletParams.readFrom(stream),
      if (StreamUtil.readBoolean(stream)) Some(model.ExplosionParams.readFrom(stream)) else None
    )
  }
}

