package model

import util.StreamUtil

case class ServerMessageGame(playerView: Option[model.PlayerView]) {
    def writeTo(stream: java.io.OutputStream) {
        playerView match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
    }
}
object ServerMessageGame {
    def readFrom(stream: java.io.InputStream): ServerMessageGame = ServerMessageGame(
        if (StreamUtil.readBoolean(stream)) Some(
            model.PlayerView.readFrom(stream)
        ) else None
        )
}
