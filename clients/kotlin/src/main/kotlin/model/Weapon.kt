package model

import util.StreamUtil

class Weapon {
    lateinit var typ: model.WeaponType
    lateinit var params: model.WeaponParams
    var magazine: Int = 0
    var wasShooting: Boolean = false
    var spread: Double = 0.0
    var fireTimer: Double? = null
    var lastAngle: Double? = null
    var lastFireTick: Int? = null
    constructor() {}
    constructor(typ: model.WeaponType, params: model.WeaponParams, magazine: Int, wasShooting: Boolean, spread: Double, fireTimer: Double?, lastAngle: Double?, lastFireTick: Int?) {
        this.typ = typ
        this.params = params
        this.magazine = magazine
        this.wasShooting = wasShooting
        this.spread = spread
        this.fireTimer = fireTimer
        this.lastAngle = lastAngle
        this.lastFireTick = lastFireTick
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Weapon {
            val result = Weapon()
            when (StreamUtil.readInt(stream)) {
            0 ->result.typ = model.WeaponType.PISTOL
            1 ->result.typ = model.WeaponType.ASSAULT_RIFLE
            2 ->result.typ = model.WeaponType.ROCKET_LAUNCHER
            else -> throw java.io.IOException("Unexpected discriminant value")
            }
            result.params = model.WeaponParams.readFrom(stream)
            result.magazine = StreamUtil.readInt(stream)
            result.wasShooting = StreamUtil.readBoolean(stream)
            result.spread = StreamUtil.readDouble(stream)
            if (StreamUtil.readBoolean(stream)) {
                result.fireTimer = StreamUtil.readDouble(stream)
            } else {
                result.fireTimer = null
            }
            if (StreamUtil.readBoolean(stream)) {
                result.lastAngle = StreamUtil.readDouble(stream)
            } else {
                result.lastAngle = null
            }
            if (StreamUtil.readBoolean(stream)) {
                result.lastFireTick = StreamUtil.readInt(stream)
            } else {
                result.lastFireTick = null
            }
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, typ.discriminant)
        params.writeTo(stream)
        StreamUtil.writeInt(stream, magazine)
        StreamUtil.writeBoolean(stream, wasShooting)
        StreamUtil.writeDouble(stream, spread)
        val fireTimer = fireTimer;
        if (fireTimer == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            StreamUtil.writeDouble(stream, fireTimer)
        }
        val lastAngle = lastAngle;
        if (lastAngle == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            StreamUtil.writeDouble(stream, lastAngle)
        }
        val lastFireTick = lastFireTick;
        if (lastFireTick == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            StreamUtil.writeInt(stream, lastFireTick)
        }
    }
}
