package model

import util.StreamUtil

case class LootBox(position: model.Vec2Double, size: model.Vec2Double, item: model.Item) {
    def writeTo(stream: java.io.OutputStream) {
        position.writeTo(stream)
        size.writeTo(stream)
        item.writeTo(stream)
    }
}
object LootBox {
    def readFrom(stream: java.io.InputStream): LootBox = LootBox(
        model.Vec2Double.readFrom(stream)
        ,
        model.Vec2Double.readFrom(stream)
        ,
        model.Item.readFrom(stream)
        )
}
