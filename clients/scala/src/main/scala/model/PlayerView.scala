package model

import util.StreamUtil

case class PlayerView(myId: Int = 0, game: model.Game = Game()) {
  def writeTo(stream: java.io.OutputStream) {
    StreamUtil.writeInt(stream, myId)
    game.writeTo(stream)
  }
}

object PlayerView {
  def readFrom(stream: java.io.InputStream): PlayerView = {
    PlayerView(
      StreamUtil.readInt(stream),
      model.Game.readFrom(stream)
    )
  }
}

