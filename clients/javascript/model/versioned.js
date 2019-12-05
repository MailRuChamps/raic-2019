const UnitAction = require('./unit-action').UnitAction;
class Versioned {
    constructor(inner) {
        this.inner = inner;
    }
    static async readFrom(stream) {
        let inner;
        inner = new Map();
        for (let i = await stream.readInt(); i > 0; i--) {
            let innerKey;
            let innerValue;
            innerKey = await stream.readInt();
            innerValue = await UnitAction.readFrom(stream);
            inner.set(innerKey, innerValue);
        }
        return new Versioned(inner);
    }
    async writeTo(stream) {
        await stream.writeInt(43981);
        let inner = this.inner;
        await stream.writeInt(inner.size);
        for (let innerEntry of inner) {
            let innerKey = innerEntry[0];
            let innerValue = innerEntry[1];
            await stream.writeInt(innerKey);
            await innerValue.writeTo(stream);
        }
    }
}
module.exports = { Versioned: Versioned }
