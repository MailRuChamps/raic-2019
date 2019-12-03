package model

import util.StreamUtil

case class Bullet(weaponType: model.WeaponType, unitId: Int, playerId: Int, position: model.Vec2Double, velocity: model.Vec2Double, damage: Int, size: Double, explosionParams: Option[model.ExplosionParams]) {
    def writeTo(stream: java.io.OutputStream) {
        weaponType.writeTo(stream)
        StreamUtil.writeInt(stream, unitId)
        StreamUtil.writeInt(stream, playerId)
        position.writeTo(stream)
        velocity.writeTo(stream)
        StreamUtil.writeInt(stream, damage)
        StreamUtil.writeDouble(stream, size)
        explosionParams match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
    }
}
object Bullet {
    def readFrom(stream: java.io.InputStream): Bullet = Bullet(
        model.WeaponType.readFrom(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        model.Vec2Double.readFrom(stream)
        ,
        model.Vec2Double.readFrom(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        if (StreamUtil.readBoolean(stream)) Some(
            model.ExplosionParams.readFrom(stream)
        ) else None
        )
}
