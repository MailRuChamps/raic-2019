package model

import util.StreamUtil

class Level {
    lateinit var tiles: Array<Array<model.Tile>>
    constructor() {}
    constructor(tiles: Array<Array<model.Tile>>) {
        this.tiles = tiles
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Level {
            val result = Level()
            result.tiles = Array(StreamUtil.readInt(stream), {
                var tilesValue: Array<model.Tile>
                tilesValue = Array(StreamUtil.readInt(stream), {
                    var tilesValueValue: model.Tile
                    when (StreamUtil.readInt(stream)) {
                    0 ->tilesValueValue = model.Tile.EMPTY
                    1 ->tilesValueValue = model.Tile.WALL
                    2 ->tilesValueValue = model.Tile.PLATFORM
                    3 ->tilesValueValue = model.Tile.LADDER
                    4 ->tilesValueValue = model.Tile.JUMP_PAD
                    else -> throw java.io.IOException("Unexpected discriminant value")
                    }
                    tilesValueValue
                })
                tilesValue
            })
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, tiles.size)
        for (tilesElement in tiles) {
            StreamUtil.writeInt(stream, tilesElement.size)
            for (tilesElementElement in tilesElement) {
                StreamUtil.writeInt(stream, tilesElementElement.discriminant)
            }
        }
    }
}
