package model

import util.StreamUtil

class ExplosionParams {
    var radius: Double = 0.0
    var damage: Int = 0
    constructor() {}
    constructor(radius: Double, damage: Int) {
        this.radius = radius
        this.damage = damage
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): ExplosionParams {
            val result = ExplosionParams()
            result.radius = StreamUtil.readDouble(stream)
            result.damage = StreamUtil.readInt(stream)
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeDouble(stream, radius)
        StreamUtil.writeInt(stream, damage)
    }
}
