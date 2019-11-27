package model

import util.StreamUtil

class ColoredVertex {
    lateinit var position: model.Vec2Float
    lateinit var color: model.ColorFloat
    constructor() {}
    constructor(position: model.Vec2Float, color: model.ColorFloat) {
        this.position = position
        this.color = color
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): ColoredVertex {
            val result = ColoredVertex()
            result.position = model.Vec2Float.readFrom(stream)
            result.color = model.ColorFloat.readFrom(stream)
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        position.writeTo(stream)
        color.writeTo(stream)
    }
}
