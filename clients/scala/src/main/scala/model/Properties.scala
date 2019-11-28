package model

import util.StreamUtil

case class Properties(maxTickCount: Int = 0,
                      teamSize: Int = 0,
                      ticksPerSecond: Double = 0.0,
                      updatesPerTick: Int = 0,
                      lootBoxSize: model.Vec2Double = Vec2Double(),
                      unitSize: model.Vec2Double = Vec2Double(),
                      unitMaxHorizontalSpeed: Double = 0.0,
                      unitFallSpeed: Double = 0.0,
                      unitJumpTime: Double = 0.0,
                      unitJumpSpeed: Double = 0.0,
                      jumpPadJumpTime: Double = 0.0,
                      jumpPadJumpSpeed: Double = 0.0,
                      unitMaxHealth: Int = 0,
                      healthPackHealth: Int = 0,
                      weaponParams: Map[model.WeaponType, model.WeaponParams] = Map.empty,
                      mineSize: model.Vec2Double = Vec2Double(),
                      mineExplosionParams: model.ExplosionParams = ExplosionParams(),
                      minePrepareTime: Double = 0.0,
                      mineTriggerTime: Double = 0.0,
                      mineTriggerRadius: Double = 0.0,
                      killScore: Int = 0) {

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
      StreamUtil.writeInt(stream, key.discriminant)
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
  def readFrom(stream: java.io.InputStream): Properties = {
    Properties(
      StreamUtil.readInt(stream),
      StreamUtil.readInt(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readInt(stream),
      model.Vec2Double.readFrom(stream),
      model.Vec2Double.readFrom(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readInt(stream),
      StreamUtil.readInt(stream),
      (0 until StreamUtil.readInt(stream)).map(_ => (WeaponType.readFrom(stream), model.WeaponParams.readFrom(stream))).toMap,
      model.Vec2Double.readFrom(stream),
      model.ExplosionParams.readFrom(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readDouble(stream),
      StreamUtil.readInt(stream))
  }
}
