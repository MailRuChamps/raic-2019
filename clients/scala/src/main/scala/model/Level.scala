package model

import util.StreamUtil

case class Level(tiles: List[List[model.Tile]] = List.empty) {
  def writeTo(stream: java.io.OutputStream) {
    StreamUtil.writeInt(stream, tiles.size)
    tiles.foreach { tilesElement =>
      StreamUtil.writeInt(stream, tilesElement.size)
      tilesElement.foreach { tilesElementElement =>
        StreamUtil.writeInt(stream, tilesElementElement.discriminant)
      }
    }
  }
}

object Level {
  def readFrom(stream: java.io.InputStream): Level = {
    Level(
      (0 until StreamUtil.readInt(stream)).map { _ =>
        (0 until StreamUtil.readInt(stream)).map { _ =>
          Tile.readFrom(stream)
        }.toList
      }.toList
    )
  }

}
