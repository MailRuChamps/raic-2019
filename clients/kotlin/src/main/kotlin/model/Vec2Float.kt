package model

import util.StreamUtil

class Vec2Float {
    var x: Float = 0.0f
    var y: Float = 0.0f
    constructor() {}
    constructor(x: Float, y: Float) {
        this.x = x
        this.y = y
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Vec2Float {
            val result = Vec2Float()
            result.x = StreamUtil.readFloat(stream)
            result.y = StreamUtil.readFloat(stream)
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeFloat(stream, x)
        StreamUtil.writeFloat(stream, y)
    }
}
