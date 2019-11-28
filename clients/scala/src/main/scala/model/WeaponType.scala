package model

import util.StreamUtil

sealed abstract class WeaponType(val discriminant: Int)

object WeaponType {
  case object PISTOL extends WeaponType(0)
  case object ASSAULT_RIFLE extends WeaponType(1)
  case object ROCKET_LAUNCHER extends WeaponType(2)

  def readFrom(stream: java.io.InputStream): WeaponType = StreamUtil.readInt(stream) match {
    case 0 => model.WeaponType.PISTOL
    case 1 => model.WeaponType.ASSAULT_RIFLE
    case 2 => model.WeaponType.ROCKET_LAUNCHER
    case _ => throw new java.io.IOException("Unexpected discriminant value")
  }
}
