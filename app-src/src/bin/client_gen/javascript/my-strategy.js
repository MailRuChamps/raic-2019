const UnitAction = require('./model/unit-action').UnitAction;
const Vec2Double = require('./model/vec2-double').Vec2Double;
const Item = require('./model/item').Item;
const Tile = require('./model/tile');
const CustomData = require('./model/custom-data').CustomData;

class MyStrategy {
    async getAction (unit, game, debug) {
        const distanceSqr = function (a, b) {
            return Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2);
        };

        let minDistance = Number.POSITIVE_INFINITY;
        const nearestEnemy = game.units
            .filter((u) => {
                return u.playerId !== unit.playerId;
            })
            .reduce(function (prev, u) {
                let currentDistance = distanceSqr(u.position, unit.position);
                if (currentDistance < minDistance) {
                    minDistance = currentDistance;
                    return u;
                }
                return prev;
            });
        
        minDistance = Number.POSITIVE_INFINITY;
        const nearestWeapon = game.lootBoxes
            .filter ((box) => {
                return box.item instanceof Item.Weapon;
            })
            .reduce(function (prev, box) {
                let currentDistance = distanceSqr(box.position, unit.position);
                if (currentDistance < minDistance) {
                    minDistance = currentDistance;
                    return box;
                }
                return prev;
            });

        let targetPos = unit.position;
        if (unit.weapon === null && nearestWeapon) {
            targetPos = nearestWeapon.position;
        } else {
            targetPos = nearestEnemy.position;
        }
        await debug.draw(new CustomData.Log(`Target pos: ${targetPos.toString()}`));
        
        let aim = new Vec2Double(0, 0);
        if (nearestEnemy) {
            aim = new Vec2Double(
                nearestEnemy.position.x - unit.position.x,
                nearestEnemy.position.y - unit.position.y
            );
        }

        let jump = targetPos.y > unit.position.y;
        if (targetPos.x > unit.position.x && game.level.tiles[parseInt(unit.position.x + 1)][parseInt(unit.position.y)] === Tile.Wall) {
            jump = true;
        }
            
        if (targetPos.x < unit.position.x && game.level.tiles[parseInt(unit.position.x - 1)][parseInt(unit.position.y)] === Tile.Wall) {
            jump = true;
        }

        return new UnitAction(
            targetPos.x - unit.position.x,
            jump,
            !jump,
            aim,
            true,
            false,
            false,
            false
        );
    }
}

module.exports.MyStrategy = MyStrategy;
