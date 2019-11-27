package model

import util.StreamUtil

class Player {
    var id: Int = 0
    var score: Int = 0
    constructor() {}
    constructor(id: Int, score: Int) {
        this.id = id
        this.score = score
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Player {
            val result = Player()
            result.id = StreamUtil.readInt(stream)
            result.score = StreamUtil.readInt(stream)
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, id)
        StreamUtil.writeInt(stream, score)
    }
}
