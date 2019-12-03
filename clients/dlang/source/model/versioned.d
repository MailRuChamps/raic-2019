import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Versioned {
    UnitAction[int] inner;
    this(UnitAction[int] inner) {
        this.inner = inner;
    }
    static Versioned readFrom(Stream reader) {
        auto result = Versioned();
        int innerSize = reader.readInt();
        result.inner.clear();
        for (int i = 0; i < innerSize; i++) {
            int innerKey;
            innerKey = reader.readInt();
            UnitAction innerValue;
            innerValue = UnitAction.readFrom(reader);
            result.inner[innerKey] = innerValue;
        }
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(43981);
        writer.write(cast(int)(inner.length));
        foreach (innerKey, innerValue; inner) {
            writer.write(innerKey);
            innerValue.writeTo(writer);
        }
    }
    string toString() const {
        return "Versioned" ~ "(" ~
            to!string(inner) ~
            ")";
    }
}
