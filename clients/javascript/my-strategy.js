const UnitAction = require('./model/unit-action').UnitAction;
const Vec2Double = require('./model/vec2double').Vec2Double;

class MyStrategy {
    async getAction (unit, game, debug) {
        // @todo strategy sample
        console.log('action!');
        return new UnitAction(1, true, false, new Vec2Double(0, 0), true, false, false);
    }
}

module.exports.MyStrategy = MyStrategy;
