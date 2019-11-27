import model.*;

public class MyStrategy {
  static double distanceSqr(Vec2Double a, Vec2Double b) {
    return (a.getX() - b.getX()) * (a.getX() - b.getX()) + (a.getY() - b.getY()) * (a.getY() - b.getY());
  }

  public UnitAction getAction(Unit unit, Game game, Debug debug) {
    Unit nearestEnemy = null;
    for (Unit other : game.getUnits()) {
      if (other.getPlayerId() != unit.getPlayerId()) {
        if (nearestEnemy == null || distanceSqr(unit.getPosition(),
            other.getPosition()) < distanceSqr(unit.getPosition(), nearestEnemy.getPosition())) {
          nearestEnemy = other;
        }
      }
    }
    LootBox nearestWeapon = null;
    for (LootBox lootBox : game.getLootBoxes()) {
      if (lootBox.getItem() instanceof Item.Weapon) {
        if (nearestWeapon == null || distanceSqr(unit.getPosition(),
            lootBox.getPosition()) < distanceSqr(unit.getPosition(), nearestWeapon.getPosition())) {
          nearestWeapon = lootBox;
        }
      }
    }
    Vec2Double targetPos = unit.getPosition();
    if (unit.getWeapon() == null && nearestWeapon != null) {
      targetPos = nearestWeapon.getPosition();
    } else if (nearestEnemy != null) {
      targetPos = nearestEnemy.getPosition();
    }
    debug.draw(new CustomData.Log("Target pos: " + targetPos));
    Vec2Double aim = new Vec2Double(0, 0);
    if (nearestEnemy != null) {
      aim = new Vec2Double(nearestEnemy.getPosition().getX() - unit.getPosition().getX(),
          nearestEnemy.getPosition().getY() - unit.getPosition().getY());
    }
    boolean jump = targetPos.getY() > unit.getPosition().getY();
    if (targetPos.getX() > unit.getPosition().getX() && game.getLevel()
        .getTiles()[(int) (unit.getPosition().getX() + 1)][(int) (unit.getPosition().getY())] == Tile.WALL) {
      jump = true;
    }
    if (targetPos.getX() < unit.getPosition().getX() && game.getLevel()
        .getTiles()[(int) (unit.getPosition().getX() - 1)][(int) (unit.getPosition().getY())] == Tile.WALL) {
      jump = true;
    }
    UnitAction action = new UnitAction();
    action.setVelocity(targetPos.getX() - unit.getPosition().getX());
    action.setJump(jump);
    action.setJumpDown(!jump);
    action.setAim(aim);
    action.setShoot(true);
    action.setSwapWeapon(false);
    action.setPlantMine(false);
    return action;
  }
}