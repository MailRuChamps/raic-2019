package model

import util.StreamUtil

case class WeaponParams(magazineSize: Int, fireRate: Double, reloadTime: Double, minSpread: Double, maxSpread: Double, recoil: Double, aimSpeed: Double, bullet: model.BulletParams, explosion: Option[model.ExplosionParams]) {
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
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
    }
}
object WeaponParams {
    def readFrom(stream: java.io.InputStream): WeaponParams = WeaponParams(
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        model.BulletParams.readFrom(stream)
        ,
        if (StreamUtil.readBoolean(stream)) Some(
            model.ExplosionParams.readFrom(stream)
        ) else None
        )
}
