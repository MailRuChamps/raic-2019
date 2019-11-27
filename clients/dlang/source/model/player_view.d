import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct PlayerView {
    int myId;
    Game game;
    this(int myId, Game game) {
        this.myId = myId;
        this.game = game;
    }
    static PlayerView readFrom(Stream reader) {
        auto result = PlayerView();
        result.myId = reader.readInt();
        result.game = Game.readFrom(reader);
        return result;
    }
    void writeTo(Stream writer) const {
        writer.write(myId);
        game.writeTo(writer);
    }
    string toString() const {
        return "PlayerView" ~ "(" ~
            to!string(myId) ~
            to!string(game) ~
            ")";
    }
}
