import model;
import debugger;
import std.typecons;
import std.conv;

class MyStrategy {
    UnitAction getAction(Unit unit, Game game, Debugger debugger) {
        double distanceSqr(Vec2Double a, Vec2Double b) {
            return (a.x - b.x) * (a.x - b.x) +
                   (a.y - b.y) * (a.y - b.y);
        }
        Nullable!Unit nearestEnemy;
        foreach (other; game.units) {
            if (other.playerId != unit.playerId) {
                if (nearestEnemy.isNull() ||
                    distanceSqr(unit.position, other.position) <
                        distanceSqr(unit.position, nearestEnemy.get.position)) {
                    nearestEnemy = other;
                }
            }
        }
        Nullable!LootBox nearestWeapon;
        foreach (lootBox; game.lootBoxes) {
            if (cast(Item.Weapon)(lootBox.item)) {
                if (nearestWeapon.isNull() ||
                    distanceSqr(unit.position, lootBox.position) <
                        distanceSqr(unit.position, nearestWeapon.get.position)) {
                    nearestWeapon = lootBox;
                }
            }
        }
        Vec2Double targetPos = unit.position;
        if (unit.weapon.isNull() && !nearestWeapon.isNull()) {
            targetPos = nearestWeapon.get.position;
        } else if (!nearestEnemy.isNull()) {
            targetPos = nearestEnemy.get.position;
        }
        debugger.draw(new CustomData.Log(
            "Target pos: " ~ to!string(targetPos)));
        Vec2Double aim = Vec2Double(0, 0);
        if (!nearestEnemy.isNull()) {
            aim = Vec2Double(
                nearestEnemy.get.position.x - unit.position.x,
                nearestEnemy.get.position.y - unit.position.y);
        }
        bool jump = targetPos.y > unit.position.y;
        if (targetPos.x > unit.position.x && game.level.tiles[(unit.position.x + 1).to!size_t][(unit.position.y).to!size_t] == Tile.Wall) {
            jump = true;
        }
        if (targetPos.x < unit.position.x && game.level.tiles[(unit.position.x - 1).to!size_t][(unit.position.y).to!size_t] == Tile.Wall) {
            jump = true;
        }
        UnitAction action;
        action.velocity = targetPos.x - unit.position.x;
        action.jump = jump;
        action.jumpDown = !jump;
        action.aim = aim;
        action.shoot = true;
        action.reload = false;
        action.swapWeapon = false;
        action.plantMine = false;
        return action;
    }
}