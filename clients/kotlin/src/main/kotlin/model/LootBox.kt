package model

import util.StreamUtil

class LootBox {
    lateinit var position: model.Vec2Double
    lateinit var size: model.Vec2Double
    lateinit var item: model.Item
    constructor() {}
    constructor(position: model.Vec2Double, size: model.Vec2Double, item: model.Item) {
        this.position = position
        this.size = size
        this.item = item
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): LootBox {
            val result = LootBox()
            result.position = model.Vec2Double.readFrom(stream)
            result.size = model.Vec2Double.readFrom(stream)
            result.item = model.Item.readFrom(stream)
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        position.writeTo(stream)
        size.writeTo(stream)
        item.writeTo(stream)
    }
}
