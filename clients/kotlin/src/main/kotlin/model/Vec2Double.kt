package model

import util.StreamUtil

class Vec2Double {
    var x: Double = 0.0
    var y: Double = 0.0
    constructor() {}
    constructor(x: Double, y: Double) {
        this.x = x
        this.y = y
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Vec2Double {
            val result = Vec2Double()
            result.x = StreamUtil.readDouble(stream)
            result.y = StreamUtil.readDouble(stream)
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeDouble(stream, x)
        StreamUtil.writeDouble(stream, y)
    }
}
