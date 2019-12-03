package model

import util.StreamUtil

case class UnitAction(velocity: Double, jump: Boolean, jumpDown: Boolean, aim: model.Vec2Double, shoot: Boolean, reload: Boolean, swapWeapon: Boolean, plantMine: Boolean) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeDouble(stream, velocity)
        StreamUtil.writeBoolean(stream, jump)
        StreamUtil.writeBoolean(stream, jumpDown)
        aim.writeTo(stream)
        StreamUtil.writeBoolean(stream, shoot)
        StreamUtil.writeBoolean(stream, reload)
        StreamUtil.writeBoolean(stream, swapWeapon)
        StreamUtil.writeBoolean(stream, plantMine)
    }
}
object UnitAction {
    def readFrom(stream: java.io.InputStream): UnitAction = UnitAction(
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        model.Vec2Double.readFrom(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readBoolean(stream)
        )
}
