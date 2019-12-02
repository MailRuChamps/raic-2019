package model

import util.StreamUtil

sealed trait Item {
    def writeTo(stream: java.io.OutputStream)
}
object Item {
    case class HealthPack(health: Int) extends Item {
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, HealthPack.TAG)
            StreamUtil.writeInt(stream, health)
        }
    }
    object HealthPack {
        val TAG: Int = 0
        def readFrom(stream: java.io.InputStream): HealthPack = HealthPack(
            StreamUtil.readInt(stream)
            )
    }

    case class Weapon(weaponType: model.WeaponType) extends Item {
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, Weapon.TAG)
            weaponType.writeTo(stream)
        }
    }
    object Weapon {
        val TAG: Int = 1
        def readFrom(stream: java.io.InputStream): Weapon = Weapon(
            model.WeaponType.readFrom(stream)
            )
    }

    case class Mine() extends Item {
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, Mine.TAG)
        }
    }
    object Mine {
        val TAG: Int = 2
        def readFrom(stream: java.io.InputStream): Mine = Mine(
            )
    }

    def readFrom(stream: java.io.InputStream): Item = {
        StreamUtil.readInt(stream) match {
            case HealthPack.TAG => HealthPack.readFrom(stream)
            case Weapon.TAG => Weapon.readFrom(stream)
            case Mine.TAG => Mine.readFrom(stream)
            case _ => throw new java.io.IOException("Unexpected discriminant value")
        }
    }
}
