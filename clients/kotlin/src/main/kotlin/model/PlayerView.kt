package model

import util.StreamUtil

class PlayerView {
    var myId: Int = 0
    lateinit var game: model.Game
    constructor() {}
    constructor(myId: Int, game: model.Game) {
        this.myId = myId
        this.game = game
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): PlayerView {
            val result = PlayerView()
            result.myId = StreamUtil.readInt(stream)
            result.game = model.Game.readFrom(stream)
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, myId)
        game.writeTo(stream)
    }
}
