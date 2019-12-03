import java.io.{BufferedInputStream, BufferedOutputStream}
import java.net.Socket

import model.PlayerMessageGame.ActionMessage
import util.StreamUtil

object Runner extends App {

  val host = if (args.length < 1) "127.0.0.1" else args(0)
  val port = if (args.length < 2) 31001 else args(1).toInt
  val token = if (args.length < 3) "0000000000000000" else args(2)

  run(host, port, token)

  def run(host: String, port: Int, token: String) {
    val socket = new Socket(host, port)
    socket.setTcpNoDelay(true)
    val inputStream = new BufferedInputStream(socket.getInputStream)
    val outputStream = new BufferedOutputStream(socket.getOutputStream)

    StreamUtil.writeString(outputStream, token)
    outputStream.flush()


    val myStrategy = new MyStrategy()
    val debug = new Debug(outputStream)
    while (true) {
      val message = model.ServerMessageGame.readFrom(inputStream)

      message.playerView match {
        case None => return
        case Some(playerView) =>
          val actions = playerView.game.units
            .filter(_.playerId == playerView.myId)
            .map(x => (x.id, myStrategy.getAction(x, playerView.game, debug)))
            .toMap

          ActionMessage(model.Versioned(actions)).writeTo(outputStream)
          outputStream.flush()
      }
    }
  }
}
