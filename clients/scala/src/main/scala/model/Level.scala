package model

import util.StreamUtil

case class Level(tiles: Seq[Seq[model.Tile]]) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, tiles.length)
        tiles.foreach { value =>
            StreamUtil.writeInt(stream, value.length)
            value.foreach { value =>
                value.writeTo(stream)
            }
        }
    }
}
object Level {
    def readFrom(stream: java.io.InputStream): Level = Level(
        (0 until StreamUtil.readInt(stream)).map { _ =>
            (0 until StreamUtil.readInt(stream)).map { _ =>
                model.Tile.readFrom(stream)
            }
        }
        )
}
