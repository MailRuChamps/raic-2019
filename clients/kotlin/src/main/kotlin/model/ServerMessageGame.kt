package model

import util.StreamUtil

class ServerMessageGame {
    var playerView: model.PlayerView? = null
    constructor() {}
    constructor(playerView: model.PlayerView?) {
        this.playerView = playerView
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): ServerMessageGame {
            val result = ServerMessageGame()
            if (StreamUtil.readBoolean(stream)) {
                result.playerView = model.PlayerView.readFrom(stream)
            } else {
                result.playerView = null
            }
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        val playerView = playerView;
        if (playerView == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            playerView.writeTo(stream)
        }
    }
}
