import model;
import stream;
import std.conv;
import std.typecons : Nullable;

abstract class PlayerMessageGame {
    abstract void writeTo(Stream writer) const;
    static PlayerMessageGame readFrom(Stream reader) {
        switch (reader.readInt()) {
            case CustomDataMessage.TAG:
                return CustomDataMessage.readFrom(reader);
            case ActionMessage.TAG:
                return ActionMessage.readFrom(reader);
            default:
                throw new Exception("Unexpected discriminant value");
        }
    }

    static class CustomDataMessage : PlayerMessageGame {
        static const int TAG = 0;
        CustomData data;
        this() {}
        this(CustomData data) {
            this.data = data;
        }
        static CustomDataMessage readFrom(Stream reader) {
            auto result = new CustomDataMessage();
            result.data = CustomData.readFrom(reader);
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            data.writeTo(writer);
        }
        override string toString() const {
            return "CustomDataMessage" ~ "(" ~
                to!string(data) ~
                ")";
        }
    }

    static class ActionMessage : PlayerMessageGame {
        static const int TAG = 1;
        UnitAction[int] action;
        this() {}
        this(UnitAction[int] action) {
            this.action = action;
        }
        static ActionMessage readFrom(Stream reader) {
            auto result = new ActionMessage();
            int actionSize = reader.readInt();
            result.action.clear();
            for (int i = 0; i < actionSize; i++) {
                int actionKey;
                actionKey = reader.readInt();
                UnitAction actionValue;
                actionValue = UnitAction.readFrom(reader);
                result.action[actionKey] = actionValue;
            }
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(cast(int)(action.length));
            foreach (actionKey, actionValue; action) {
                writer.write(actionKey);
                actionValue.writeTo(writer);
            }
        }
        override string toString() const {
            return "ActionMessage" ~ "(" ~
                to!string(action) ~
                ")";
        }
    }
}
