package model

import util.StreamUtil

case class Unit(playerId: Int, id: Int, health: Int, position: model.Vec2Double, size: model.Vec2Double, jumpState: model.JumpState, walkedRight: Boolean, stand: Boolean, onGround: Boolean, onLadder: Boolean, mines: Int, weapon: Option[model.Weapon]) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, playerId)
        StreamUtil.writeInt(stream, id)
        StreamUtil.writeInt(stream, health)
        position.writeTo(stream)
        size.writeTo(stream)
        jumpState.writeTo(stream)
        StreamUtil.writeBoolean(stream, walkedRight)
        StreamUtil.writeBoolean(stream, stand)
        StreamUtil.writeBoolean(stream, onGround)
        StreamUtil.writeBoolean(stream, onLadder)
        StreamUtil.writeInt(stream, mines)
        weapon match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
    }
}
object Unit {
    def readFrom(stream: java.io.InputStream): Unit = Unit(
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        model.Vec2Double.readFrom(stream)
        ,
        model.Vec2Double.readFrom(stream)
        ,
        model.JumpState.readFrom(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readInt(stream)
        ,
        if (StreamUtil.readBoolean(stream)) Some(
            model.Weapon.readFrom(stream)
        ) else None
        )
}
