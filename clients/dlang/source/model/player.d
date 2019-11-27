import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Player {
    int id;
    int score;
    this(int id, int score) {
        this.id = id;
        this.score = score;
    }
    static Player readFrom(Stream reader) {
        auto result = Player();
        result.id = reader.readInt();
        result.score = reader.readInt();
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(id);
        writer.write(score);
    }
    string toString() const {
        return "Player" ~ "(" ~
            to!string(id) ~
            to!string(score) ~
            ")";
    }
}
