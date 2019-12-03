package model

import util.StreamUtil

case class Player(id: Int, score: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, id)
        StreamUtil.writeInt(stream, score)
    }
}
object Player {
    def readFrom(stream: java.io.InputStream): Player = Player(
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readInt(stream)
        )
}
