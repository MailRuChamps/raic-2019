package model

import util.StreamUtil

class WeaponParams {
    var magazineSize: Int = 0
    var fireRate: Double = 0.0
    var reloadTime: Double = 0.0
    var minSpread: Double = 0.0
    var maxSpread: Double = 0.0
    var recoil: Double = 0.0
    var aimSpeed: Double = 0.0
    lateinit var bullet: model.BulletParams
    var explosion: model.ExplosionParams? = null
    constructor() {}
    constructor(magazineSize: Int, fireRate: Double, reloadTime: Double, minSpread: Double, maxSpread: Double, recoil: Double, aimSpeed: Double, bullet: model.BulletParams, explosion: model.ExplosionParams?) {
        this.magazineSize = magazineSize
        this.fireRate = fireRate
        this.reloadTime = reloadTime
        this.minSpread = minSpread
        this.maxSpread = maxSpread
        this.recoil = recoil
        this.aimSpeed = aimSpeed
        this.bullet = bullet
        this.explosion = explosion
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): WeaponParams {
            val result = WeaponParams()
            result.magazineSize = StreamUtil.readInt(stream)
            result.fireRate = StreamUtil.readDouble(stream)
            result.reloadTime = StreamUtil.readDouble(stream)
            result.minSpread = StreamUtil.readDouble(stream)
            result.maxSpread = StreamUtil.readDouble(stream)
            result.recoil = StreamUtil.readDouble(stream)
            result.aimSpeed = StreamUtil.readDouble(stream)
            result.bullet = model.BulletParams.readFrom(stream)
            if (StreamUtil.readBoolean(stream)) {
                result.explosion = model.ExplosionParams.readFrom(stream)
            } else {
                result.explosion = null
            }
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, magazineSize)
        StreamUtil.writeDouble(stream, fireRate)
        StreamUtil.writeDouble(stream, reloadTime)
        StreamUtil.writeDouble(stream, minSpread)
        StreamUtil.writeDouble(stream, maxSpread)
        StreamUtil.writeDouble(stream, recoil)
        StreamUtil.writeDouble(stream, aimSpeed)
        bullet.writeTo(stream)
        val explosion = explosion;
        if (explosion == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            explosion.writeTo(stream)
        }
    }
}
