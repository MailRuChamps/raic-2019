package model

import util.StreamUtil

sealed trait Item {
  def writeTo(stream: java.io.OutputStream)
}

object Item {
  case class HealthPack(health: Int = 0) extends Item {
    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, HealthPack.tag)
      StreamUtil.writeInt(stream, health)
    }
  }

  object HealthPack extends Tagged {
    override val tag: Int = 0

    def readFrom(stream: java.io.InputStream): HealthPack = {
      HealthPack(StreamUtil.readInt(stream))
    }
  }

  case class Weapon(weaponType: model.WeaponType) extends Item {
    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, Weapon.tag)
      StreamUtil.writeInt(stream, weaponType.discriminant)
    }
  }

  object Weapon extends Tagged {
    override val tag: Int = 1

    def readFrom(stream: java.io.InputStream): Weapon = {
      Weapon(WeaponType.readFrom(stream))
    }
  }

  case object Mine extends Item with Tagged {
    override val tag: Int = 1

    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, Mine.tag)
    }
  }

  def readFrom(stream: java.io.InputStream): Item = {
    StreamUtil.readInt(stream) match {
      case HealthPack.tag => HealthPack.readFrom(stream)
      case Weapon.tag => Weapon.readFrom(stream)
      case Mine.tag => Mine
      case _ => throw new java.io.IOException("Unexpected discriminant value")
    }
  }
}

