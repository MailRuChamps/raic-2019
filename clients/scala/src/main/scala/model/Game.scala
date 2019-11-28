package model

import util.StreamUtil

case class Game(currentTick: Int = 0,
                properties: model.Properties = Properties(),
                level: model.Level = Level(),
                players: Seq[model.Player] = Seq.empty,
                units: Seq[model.Unit] = Seq.empty,
                bullets: Seq[model.Bullet] = Seq.empty,
                mines: Seq[model.Mine] = Seq.empty,
                lootBoxes: Seq[model.LootBox] = Seq.empty) {
  def writeTo(stream: java.io.OutputStream) {
    StreamUtil.writeInt(stream, currentTick)
    properties.writeTo(stream)
    level.writeTo(stream)
    StreamUtil.writeInt(stream, players.size)
    players.foreach(_.writeTo(stream))
    StreamUtil.writeInt(stream, units.size)
    units.foreach(_.writeTo(stream))
    StreamUtil.writeInt(stream, bullets.size)
    bullets.foreach(_.writeTo(stream))
    StreamUtil.writeInt(stream, mines.size)
    mines.foreach(_.writeTo(stream))
    StreamUtil.writeInt(stream, lootBoxes.size)
    lootBoxes.foreach(_.writeTo(stream))
  }
}

object Game {

  def readFrom(stream: java.io.InputStream): Game = {
    Game(
      StreamUtil.readInt(stream),
      model.Properties.readFrom(stream),
      model.Level.readFrom(stream),
      (0 until StreamUtil.readInt(stream)).map { _ => model.Player.readFrom(stream) },
      (0 until StreamUtil.readInt(stream)).map { _ => model.Unit.readFrom(stream) },
      (0 until StreamUtil.readInt(stream)).map { _ => model.Bullet.readFrom(stream) },
      (0 until StreamUtil.readInt(stream)).map { _ => model.Mine.readFrom(stream) },
      (0 until StreamUtil.readInt(stream)).map { _ => model.LootBox.readFrom(stream) }
    )
  }

}