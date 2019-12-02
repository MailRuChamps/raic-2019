package model

import util.StreamUtil

case class Weapon(typ: model.WeaponType, params: model.WeaponParams, magazine: Int, wasShooting: Boolean, spread: Double, fireTimer: Option[Double], lastAngle: Option[Double], lastFireTick: Option[Int]) {
    def writeTo(stream: java.io.OutputStream) {
        typ.writeTo(stream)
        params.writeTo(stream)
        StreamUtil.writeInt(stream, magazine)
        StreamUtil.writeBoolean(stream, wasShooting)
        StreamUtil.writeDouble(stream, spread)
        fireTimer match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                StreamUtil.writeDouble(stream, value)
            }
        }
        lastAngle match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                StreamUtil.writeDouble(stream, value)
            }
        }
        lastFireTick match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                StreamUtil.writeInt(stream, value)
            }
        }
    }
}
object Weapon {
    def readFrom(stream: java.io.InputStream): Weapon = Weapon(
        model.WeaponType.readFrom(stream)
        ,
        model.WeaponParams.readFrom(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readDouble(stream)
        ) else None
        ,
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readDouble(stream)
        ) else None
        ,
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readInt(stream)
        ) else None
        )
}
