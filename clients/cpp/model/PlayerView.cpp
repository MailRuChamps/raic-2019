#include "PlayerView.hpp"

PlayerView::PlayerView() { }
PlayerView::PlayerView(int myId, Game game) : myId(myId), game(game) { }
PlayerView PlayerView::readFrom(InputStream& stream) {
    PlayerView result;
    result.myId = stream.readInt();
    result.game = Game::readFrom(stream);
    return result;
}
void PlayerView::writeTo(OutputStream& stream) const {
    stream.write(myId);
    game.writeTo(stream);
}
std::string PlayerView::toString() const {
    return std::string("PlayerView") + "(" +
        std::to_string(myId) +
        game.toString() +
        ")";
}
