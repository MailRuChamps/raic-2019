import model.CustomData.Log
import model.{Tile, UnitAction, Vec2Double}

class MyStrategy {
  def getAction(unit: model.Unit, game: model.Game, debug: Debug): model.UnitAction = {
    val nearestEnemy = game.units.filter(_.playerId != unit.playerId)
      .sortBy(x => distanceSqr(unit.position, x.position))
      .headOption

    val nearestWeapon = game.lootBoxes
      .sortBy(x => distanceSqr(unit.position, x.position))
      .headOption

    var targetPos: Vec2Double = unit.position
    if (unit.weapon.isEmpty && nearestWeapon.isDefined) {
      targetPos = nearestWeapon.get.position
    } else if (nearestEnemy.isDefined) {
      targetPos = nearestEnemy.get.position
    }

    debug.draw(Log(s"Target pos: $targetPos"))
    val aim = nearestEnemy match {
      case Some(enemy) =>
        Vec2Double(enemy.position.x - unit.position.x,
          enemy.position.y - unit.position.y)
      case None => Vec2Double(0, 0)
    }

    var jump = targetPos.y > unit.position.y
    if (targetPos.x > unit.position.x && game.level.tiles((unit.position.x + 1).toInt)(unit.position.y.toInt) == Tile.WALL) {
      jump = true
    }
    if (targetPos.x < unit.position.x && game.level.tiles((unit.position.x - 1).toInt)(unit.position.y.toInt) == Tile.WALL) {
      jump = true
    }

    UnitAction(velocity = targetPos.x - unit.position.x,
      jump = jump,
      jumpDown = !jump,
      aim = aim,
      shoot = true,
      reload = false,
      swapWeapon = false,
      plantMine = false)
  }

  private def distanceSqr(a: Vec2Double, b: Vec2Double): Double = {
    (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y)
  }
}