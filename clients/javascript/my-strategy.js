const UnitAction = require('./model/unit-action').UnitAction;
const Vec2Double = require('./model/vec2double').Vec2Double;

class MyStrategy {
    async getAction (unit, game, debug) {
        // @todo strategy sample
        console.log('action!');
        return new UnitAction(0.0, false, false, new Vec2Double(0, 0), false, false, false);
    }
}

module.exports.MyStrategy = MyStrategy;
