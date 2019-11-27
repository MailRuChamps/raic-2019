#include "Player.hpp"

Player::Player() { }
Player::Player(int id, int score) : id(id), score(score) { }
Player Player::readFrom(InputStream& stream) {
    Player result;
    result.id = stream.readInt();
    result.score = stream.readInt();
    return result;
}
void Player::writeTo(OutputStream& stream) const {
    stream.write(id);
    stream.write(score);
}
bool Player::operator ==(const Player& other) const {
    return id == other.id && score == other.score;
}
size_t std::hash<Player>::operator ()(const Player& value) const {
    size_t result = 0;
    result ^= std::hash<int>{}(value.id) + 0x9e3779b9 + (result<<6) + (result>>2);
    result ^= std::hash<int>{}(value.score) + 0x9e3779b9 + (result<<6) + (result>>2);
    return result;
}
std::string Player::toString() const {
    return std::string("Player") + "(" +
        std::to_string(id) +
        std::to_string(score) +
        ")";
}
