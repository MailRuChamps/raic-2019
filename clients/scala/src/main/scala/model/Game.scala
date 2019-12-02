package model

import util.StreamUtil

case class Game(currentTick: Int, properties: model.Properties, level: model.Level, players: Seq[model.Player], units: Seq[model.Unit], bullets: Seq[model.Bullet], mines: Seq[model.Mine], lootBoxes: Seq[model.LootBox]) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, currentTick)
        properties.writeTo(stream)
        level.writeTo(stream)
        StreamUtil.writeInt(stream, players.length)
        players.foreach { value =>
            value.writeTo(stream)
        }
        StreamUtil.writeInt(stream, units.length)
        units.foreach { value =>
            value.writeTo(stream)
        }
        StreamUtil.writeInt(stream, bullets.length)
        bullets.foreach { value =>
            value.writeTo(stream)
        }
        StreamUtil.writeInt(stream, mines.length)
        mines.foreach { value =>
            value.writeTo(stream)
        }
        StreamUtil.writeInt(stream, lootBoxes.length)
        lootBoxes.foreach { value =>
            value.writeTo(stream)
        }
    }
}
object Game {
    def readFrom(stream: java.io.InputStream): Game = Game(
        StreamUtil.readInt(stream)
        ,
        model.Properties.readFrom(stream)
        ,
        model.Level.readFrom(stream)
        ,
        (0 until StreamUtil.readInt(stream)).map { _ =>
            model.Player.readFrom(stream)
        }
        ,
        (0 until StreamUtil.readInt(stream)).map { _ =>
            model.Unit.readFrom(stream)
        }
        ,
        (0 until StreamUtil.readInt(stream)).map { _ =>
            model.Bullet.readFrom(stream)
        }
        ,
        (0 until StreamUtil.readInt(stream)).map { _ =>
            model.Mine.readFrom(stream)
        }
        ,
        (0 until StreamUtil.readInt(stream)).map { _ =>
            model.LootBox.readFrom(stream)
        }
        )
}
