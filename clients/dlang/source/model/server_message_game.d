import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct ServerMessageGame {
    Nullable!(PlayerView) playerView;
    this(Nullable!(PlayerView) playerView) {
        this.playerView = playerView;
    }
    static ServerMessageGame readFrom(Stream reader) {
        auto result = ServerMessageGame();
        if (reader.readBool()) {
            result.playerView = PlayerView.readFrom(reader);
        } else {
            result.playerView.nullify();
        }
        return result;
    }
    void writeTo(Stream writer) const {
        if (playerView.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            playerView.get.writeTo(writer);
        }
    }
    string toString() const {
        return "ServerMessageGame" ~ "(" ~
            to!string(playerView) ~
            ")";
    }
}
