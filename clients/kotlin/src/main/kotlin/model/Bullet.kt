package model

import util.StreamUtil

class Bullet {
    lateinit var weaponType: model.WeaponType
    var unitId: Int = 0
    var playerId: Int = 0
    lateinit var position: model.Vec2Double
    lateinit var velocity: model.Vec2Double
    var damage: Int = 0
    var size: Double = 0.0
    var explosionParams: model.ExplosionParams? = null
    constructor() {}
    constructor(weaponType: model.WeaponType, unitId: Int, playerId: Int, position: model.Vec2Double, velocity: model.Vec2Double, damage: Int, size: Double, explosionParams: model.ExplosionParams?) {
        this.weaponType = weaponType
        this.unitId = unitId
        this.playerId = playerId
        this.position = position
        this.velocity = velocity
        this.damage = damage
        this.size = size
        this.explosionParams = explosionParams
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Bullet {
            val result = Bullet()
            when (StreamUtil.readInt(stream)) {
            0 ->result.weaponType = model.WeaponType.PISTOL
            1 ->result.weaponType = model.WeaponType.ASSAULT_RIFLE
            2 ->result.weaponType = model.WeaponType.ROCKET_LAUNCHER
            else -> throw java.io.IOException("Unexpected discriminant value")
            }
            result.unitId = StreamUtil.readInt(stream)
            result.playerId = StreamUtil.readInt(stream)
            result.position = model.Vec2Double.readFrom(stream)
            result.velocity = model.Vec2Double.readFrom(stream)
            result.damage = StreamUtil.readInt(stream)
            result.size = StreamUtil.readDouble(stream)
            if (StreamUtil.readBoolean(stream)) {
                result.explosionParams = model.ExplosionParams.readFrom(stream)
            } else {
                result.explosionParams = null
            }
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, weaponType.discriminant)
        StreamUtil.writeInt(stream, unitId)
        StreamUtil.writeInt(stream, playerId)
        position.writeTo(stream)
        velocity.writeTo(stream)
        StreamUtil.writeInt(stream, damage)
        StreamUtil.writeDouble(stream, size)
        val explosionParams = explosionParams;
        if (explosionParams == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            explosionParams.writeTo(stream)
        }
    }
}
