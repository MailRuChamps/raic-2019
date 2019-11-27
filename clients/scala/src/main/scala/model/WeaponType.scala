package model

sealed abstract class WeaponType(val discriminant: Int)

object WeaponType {
  case object PISTOL extends WeaponType(0)
  case object ASSAULT_RIFLE extends WeaponType(1)
  case object ROCKET_LAUNCHER extends WeaponType(2)
}
