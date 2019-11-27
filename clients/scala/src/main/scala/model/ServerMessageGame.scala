package model

import util.StreamUtil

case class ServerMessageGame(playerView: Option[model.PlayerView] = None) {

  def writeTo(stream: java.io.OutputStream) {
    playerView match {
      case Some(v) =>
        StreamUtil.writeBoolean(stream, true)
        v.writeTo(stream)
      case None =>
        StreamUtil.writeBoolean(stream, false)
    }
  }
}

object ServerMessageGame {
  def readFrom(stream: java.io.InputStream): ServerMessageGame = {
    ServerMessageGame(
      if (StreamUtil.readBoolean(stream)) Some(model.PlayerView.readFrom(stream)) else None
    )
  }
}
