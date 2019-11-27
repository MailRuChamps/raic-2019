package model

import util.StreamUtil

case class Unit(
                 playerId: Int = 0,
                 id: Int = 0,
                 health: Int = 0,
                 position: model.Vec2Double = model.Vec2Double(),
                 size: model.Vec2Double = model.Vec2Double(),
                 jumpState: model.JumpState = model.JumpState(),
                 walkedRight: Boolean = false,
                 stand: Boolean = false,
                 onGround: Boolean = false,
                 onLadder: Boolean = false,
                 mines: Int = 0,
                 weapon: Option[model.Weapon] = None) {

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
      case Some(w) =>
        StreamUtil.writeBoolean(stream, true)
        w.writeTo(stream)
      case None =>
        StreamUtil.writeBoolean(stream, false)
    }
  }
}

object Unit {
  def readFrom(stream: java.io.InputStream): Unit = {
    Unit(
      StreamUtil.readInt(stream),
      StreamUtil.readInt(stream),
      StreamUtil.readInt(stream),
      model.Vec2Double.readFrom(stream),
      model.Vec2Double.readFrom(stream),
      model.JumpState.readFrom(stream),
      StreamUtil.readBoolean(stream),
      StreamUtil.readBoolean(stream),
      StreamUtil.readBoolean(stream),
      StreamUtil.readBoolean(stream),
      StreamUtil.readInt(stream),
      if (StreamUtil.readBoolean(stream)) Some(model.Weapon.readFrom(stream)) else None
    )
  }
}
