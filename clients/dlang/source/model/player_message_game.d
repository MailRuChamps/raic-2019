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
        Versioned action;
        this() {}
        this(Versioned action) {
            this.action = action;
        }
        static ActionMessage readFrom(Stream reader) {
            auto result = new ActionMessage();
            result.action = Versioned.readFrom(reader);
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            action.writeTo(writer);
        }
        override string toString() const {
            return "ActionMessage" ~ "(" ~
                to!string(action) ~
                ")";
        }
    }
}
