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
      (0 to StreamUtil.readInt(stream)).map { _ =>
        (0 to StreamUtil.readInt(stream)).map { _ =>
          StreamUtil.readInt(stream) match {
            case 0 => model.Tile.EMPTY
            case 1 => model.Tile.WALL
            case 2 => model.Tile.PLATFORM
            case 3 => model.Tile.LADDER
            case 4 => model.Tile.JUMP_PAD
            case _ => throw new java.io.IOException("Unexpected discriminant value")
          }
        }.toList
      }.toList
    )
  }

}
