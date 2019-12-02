package model

import util.StreamUtil

case class Properties(maxTickCount: Int, teamSize: Int, ticksPerSecond: Double, updatesPerTick: Int, lootBoxSize: model.Vec2Double, unitSize: model.Vec2Double, unitMaxHorizontalSpeed: Double, unitFallSpeed: Double, unitJumpTime: Double, unitJumpSpeed: Double, jumpPadJumpTime: Double, jumpPadJumpSpeed: Double, unitMaxHealth: Int, healthPackHealth: Int, weaponParams: Map[model.WeaponType, model.WeaponParams], mineSize: model.Vec2Double, mineExplosionParams: model.ExplosionParams, minePrepareTime: Double, mineTriggerTime: Double, mineTriggerRadius: Double, killScore: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, maxTickCount)
        StreamUtil.writeInt(stream, teamSize)
        StreamUtil.writeDouble(stream, ticksPerSecond)
        StreamUtil.writeInt(stream, updatesPerTick)
        lootBoxSize.writeTo(stream)
        unitSize.writeTo(stream)
        StreamUtil.writeDouble(stream, unitMaxHorizontalSpeed)
        StreamUtil.writeDouble(stream, unitFallSpeed)
        StreamUtil.writeDouble(stream, unitJumpTime)
        StreamUtil.writeDouble(stream, unitJumpSpeed)
        StreamUtil.writeDouble(stream, jumpPadJumpTime)
        StreamUtil.writeDouble(stream, jumpPadJumpSpeed)
        StreamUtil.writeInt(stream, unitMaxHealth)
        StreamUtil.writeInt(stream, healthPackHealth)
        StreamUtil.writeInt(stream, weaponParams.size)
        weaponParams.foreach { case (key, value) =>
            key.writeTo(stream)
            value.writeTo(stream)
        }
        mineSize.writeTo(stream)
        mineExplosionParams.writeTo(stream)
        StreamUtil.writeDouble(stream, minePrepareTime)
        StreamUtil.writeDouble(stream, mineTriggerTime)
        StreamUtil.writeDouble(stream, mineTriggerRadius)
        StreamUtil.writeInt(stream, killScore)
    }
}
object Properties {
    def readFrom(stream: java.io.InputStream): Properties = Properties(
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        model.Vec2Double.readFrom(stream)
        ,
        model.Vec2Double.readFrom(stream)
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
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        (0 until StreamUtil.readInt(stream)).map { _ => (
            model.WeaponType.readFrom(stream)
            ,
            model.WeaponParams.readFrom(stream)
        )}.toMap
        ,
        model.Vec2Double.readFrom(stream)
        ,
        model.ExplosionParams.readFrom(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readInt(stream)
        )
}
