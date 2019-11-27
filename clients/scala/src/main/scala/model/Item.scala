package model

import util.StreamUtil

abstract class Item {
  def writeTo(stream: java.io.OutputStream)
}

object Item {
  case class HealthPack(health: Int = 0) extends Item {
    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, HealthPack.TAG)
      StreamUtil.writeInt(stream, health)
    }
  }

  object HealthPack {
    val TAG = 0

    def readFrom(stream: java.io.InputStream): HealthPack = {
      HealthPack(StreamUtil.readInt(stream))
    }
  }

  case class Weapon(weaponType: model.WeaponType) extends Item {
    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, Weapon.TAG)
      StreamUtil.writeInt(stream, weaponType.discriminant)
    }
  }

  object Weapon {
    val TAG = 1

    def readFrom(stream: java.io.InputStream): Weapon = {
      val weaponType = StreamUtil.readInt(stream) match {
        case 0 => model.WeaponType.PISTOL
        case 1 => model.WeaponType.ASSAULT_RIFLE
        case 2 => model.WeaponType.ROCKET_LAUNCHER
        case _ => throw new java.io.IOException("Unexpected discriminant value")
      }
      Weapon(weaponType)
    }
  }

  case object Mine extends Item {
    val TAG = 2

    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, Mine.TAG)
    }
  }

  def readFrom(stream: java.io.InputStream): Item = {
    StreamUtil.readInt(stream) match {
      case HealthPack.TAG => HealthPack.readFrom(stream)
      case Weapon.TAG => Weapon.readFrom(stream)
      case Mine.TAG => Mine
      case _ => throw new java.io.IOException("Unexpected discriminant value")
    }
  }
}

