const UnitAction = require('./unit-action').UnitAction;

class Versioned {
    constructor (inner) {
        this.inner = inner;
    }
    
    static async readFrom (stream) {
        const inner = {};
        const innerSize = await stream.readInt();
        for (let i = 0; i < innerSize; i++) {
            let innerKey = await stream.readInt();
            let innerValue = await UnitAction.readFrom(stream);
            inner[innerKey] = innerValue;
        }
        return new Versioned(inner);
    }

    async writeTo (stream) {
        const innerKeys = Object.keys(this.inner);
        const innerKeysSize = innerKeys.length;
        await stream.writeInt(43981);
        await stream.writeInt(innerKeysSize);

        for (let i = 0; i < innerKeysSize; i++) {
            let key = innerKeys[i];
            await stream.writeInt(key);
            await this.inner[key].writeTo(stream);
        }
    }

    toString () {
        return 'Versioned(' +
            this.inner +
            ')';
    }
}

module.exports.Versioned = Versioned;
