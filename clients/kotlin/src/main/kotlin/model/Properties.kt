package model

import util.StreamUtil

class Properties {
    var maxTickCount: Int = 0
    var teamSize: Int = 0
    var ticksPerSecond: Double = 0.0
    var updatesPerTick: Int = 0
    lateinit var lootBoxSize: model.Vec2Double
    lateinit var unitSize: model.Vec2Double
    var unitMaxHorizontalSpeed: Double = 0.0
    var unitFallSpeed: Double = 0.0
    var unitJumpTime: Double = 0.0
    var unitJumpSpeed: Double = 0.0
    var jumpPadJumpTime: Double = 0.0
    var jumpPadJumpSpeed: Double = 0.0
    var unitMaxHealth: Int = 0
    var healthPackHealth: Int = 0
    lateinit var weaponParams: MutableMap<model.WeaponType, model.WeaponParams>
    lateinit var mineSize: model.Vec2Double
    lateinit var mineExplosionParams: model.ExplosionParams
    var minePrepareTime: Double = 0.0
    var mineTriggerTime: Double = 0.0
    var mineTriggerRadius: Double = 0.0
    var killScore: Int = 0
    constructor() {}
    constructor(maxTickCount: Int, teamSize: Int, ticksPerSecond: Double, updatesPerTick: Int, lootBoxSize: model.Vec2Double, unitSize: model.Vec2Double, unitMaxHorizontalSpeed: Double, unitFallSpeed: Double, unitJumpTime: Double, unitJumpSpeed: Double, jumpPadJumpTime: Double, jumpPadJumpSpeed: Double, unitMaxHealth: Int, healthPackHealth: Int, weaponParams: MutableMap<model.WeaponType, model.WeaponParams>, mineSize: model.Vec2Double, mineExplosionParams: model.ExplosionParams, minePrepareTime: Double, mineTriggerTime: Double, mineTriggerRadius: Double, killScore: Int) {
        this.maxTickCount = maxTickCount
        this.teamSize = teamSize
        this.ticksPerSecond = ticksPerSecond
        this.updatesPerTick = updatesPerTick
        this.lootBoxSize = lootBoxSize
        this.unitSize = unitSize
        this.unitMaxHorizontalSpeed = unitMaxHorizontalSpeed
        this.unitFallSpeed = unitFallSpeed
        this.unitJumpTime = unitJumpTime
        this.unitJumpSpeed = unitJumpSpeed
        this.jumpPadJumpTime = jumpPadJumpTime
        this.jumpPadJumpSpeed = jumpPadJumpSpeed
        this.unitMaxHealth = unitMaxHealth
        this.healthPackHealth = healthPackHealth
        this.weaponParams = weaponParams
        this.mineSize = mineSize
        this.mineExplosionParams = mineExplosionParams
        this.minePrepareTime = minePrepareTime
        this.mineTriggerTime = mineTriggerTime
        this.mineTriggerRadius = mineTriggerRadius
        this.killScore = killScore
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Properties {
            val result = Properties()
            result.maxTickCount = StreamUtil.readInt(stream)
            result.teamSize = StreamUtil.readInt(stream)
            result.ticksPerSecond = StreamUtil.readDouble(stream)
            result.updatesPerTick = StreamUtil.readInt(stream)
            result.lootBoxSize = model.Vec2Double.readFrom(stream)
            result.unitSize = model.Vec2Double.readFrom(stream)
            result.unitMaxHorizontalSpeed = StreamUtil.readDouble(stream)
            result.unitFallSpeed = StreamUtil.readDouble(stream)
            result.unitJumpTime = StreamUtil.readDouble(stream)
            result.unitJumpSpeed = StreamUtil.readDouble(stream)
            result.jumpPadJumpTime = StreamUtil.readDouble(stream)
            result.jumpPadJumpSpeed = StreamUtil.readDouble(stream)
            result.unitMaxHealth = StreamUtil.readInt(stream)
            result.healthPackHealth = StreamUtil.readInt(stream)
            val weaponParamsSize = StreamUtil.readInt(stream)
            result.weaponParams = mutableMapOf()
            for (i in 0 until weaponParamsSize) {
                var weaponParamsKey: model.WeaponType
                when (StreamUtil.readInt(stream)) {
                0 ->weaponParamsKey = model.WeaponType.PISTOL
                1 ->weaponParamsKey = model.WeaponType.ASSAULT_RIFLE
                2 ->weaponParamsKey = model.WeaponType.ROCKET_LAUNCHER
                else -> throw java.io.IOException("Unexpected discriminant value")
                }
                var weaponParamsValue: model.WeaponParams
                weaponParamsValue = model.WeaponParams.readFrom(stream)
                result.weaponParams.put(weaponParamsKey, weaponParamsValue)
            }
            result.mineSize = model.Vec2Double.readFrom(stream)
            result.mineExplosionParams = model.ExplosionParams.readFrom(stream)
            result.minePrepareTime = StreamUtil.readDouble(stream)
            result.mineTriggerTime = StreamUtil.readDouble(stream)
            result.mineTriggerRadius = StreamUtil.readDouble(stream)
            result.killScore = StreamUtil.readInt(stream)
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
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
        for (weaponParamsEntry in weaponParams) {
            StreamUtil.writeInt(stream, weaponParamsEntry.key.discriminant)
            weaponParamsEntry.value.writeTo(stream)
        }
        mineSize.writeTo(stream)
        mineExplosionParams.writeTo(stream)
        StreamUtil.writeDouble(stream, minePrepareTime)
        StreamUtil.writeDouble(stream, mineTriggerTime)
        StreamUtil.writeDouble(stream, mineTriggerRadius)
        StreamUtil.writeInt(stream, killScore)
    }
}
