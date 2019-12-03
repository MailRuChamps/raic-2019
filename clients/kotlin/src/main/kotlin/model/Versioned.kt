package model

import util.StreamUtil

class Versioned {
    lateinit var inner: MutableMap<Int, model.UnitAction>
    constructor() {}
    constructor(inner: MutableMap<Int, model.UnitAction>) {
        this.inner = inner
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Versioned {
            val result = Versioned()
            val innerSize = StreamUtil.readInt(stream)
            result.inner = mutableMapOf()
            for (i in 0 until innerSize) {
                var innerKey: Int
                innerKey = StreamUtil.readInt(stream)
                var innerValue: model.UnitAction
                innerValue = model.UnitAction.readFrom(stream)
                result.inner.put(innerKey, innerValue)
            }
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, 43981)
        StreamUtil.writeInt(stream, inner.size)
        for (innerEntry in inner) {
            StreamUtil.writeInt(stream, innerEntry.key)
            innerEntry.value.writeTo(stream)
        }
    }
}
