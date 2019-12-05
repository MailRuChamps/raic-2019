const CustomData = require('./custom-data').CustomData;
const Versioned = require('./versioned').Versioned;
class PlayerMessageGame {
    static async readFrom(stream) {
        let discriminant = await stream.readInt();
        if (discriminant == CustomDataMessage.TAG) {
            return await CustomDataMessage.readFrom(stream);
        }
        if (discriminant == ActionMessage.TAG) {
            return await ActionMessage.readFrom(stream);
        }
        throw new Error("Unexpected discriminant value");
    }
}

class CustomDataMessage extends PlayerMessageGame {
    constructor(data) {
        super();
        this.data = data;
    }
    static async readFrom(stream) {
        let data;
        data = await CustomData.readFrom(stream);
        return new CustomDataMessage(data);
    }
    async writeTo(stream) {
        await stream.writeInt(CustomDataMessage.TAG);
        let data = this.data;
        await data.writeTo(stream);
    }
}
PlayerMessageGame.CustomDataMessage = CustomDataMessage;
CustomDataMessage.TAG = 0;
class ActionMessage extends PlayerMessageGame {
    constructor(action) {
        super();
        this.action = action;
    }
    static async readFrom(stream) {
        let action;
        action = await Versioned.readFrom(stream);
        return new ActionMessage(action);
    }
    async writeTo(stream) {
        await stream.writeInt(ActionMessage.TAG);
        let action = this.action;
        await action.writeTo(stream);
    }
}
PlayerMessageGame.ActionMessage = ActionMessage;
ActionMessage.TAG = 1;
module.exports = { PlayerMessageGame: PlayerMessageGame }
