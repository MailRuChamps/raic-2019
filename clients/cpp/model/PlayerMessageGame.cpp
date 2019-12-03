#include "PlayerMessageGame.hpp"


PlayerMessageGame::CustomDataMessage::CustomDataMessage() { }
PlayerMessageGame::CustomDataMessage::CustomDataMessage(std::shared_ptr<CustomData> data) : data(data) { }
PlayerMessageGame::CustomDataMessage PlayerMessageGame::CustomDataMessage::readFrom(InputStream& stream) {
    PlayerMessageGame::CustomDataMessage result;
    result.data = CustomData::readFrom(stream);
    return result;
}
void PlayerMessageGame::CustomDataMessage::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    data->writeTo(stream);
}
std::string PlayerMessageGame::CustomDataMessage::toString() const {
    return std::string("PlayerMessageGame::CustomDataMessage") + "(" +
        data->toString() +
        ")";
}

PlayerMessageGame::ActionMessage::ActionMessage() { }
PlayerMessageGame::ActionMessage::ActionMessage(Versioned action) : action(action) { }
PlayerMessageGame::ActionMessage PlayerMessageGame::ActionMessage::readFrom(InputStream& stream) {
    PlayerMessageGame::ActionMessage result;
    result.action = Versioned::readFrom(stream);
    return result;
}
void PlayerMessageGame::ActionMessage::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    action.writeTo(stream);
}
std::string PlayerMessageGame::ActionMessage::toString() const {
    return std::string("PlayerMessageGame::ActionMessage") + "(" +
        action.toString() +
        ")";
}
std::shared_ptr<PlayerMessageGame> PlayerMessageGame::readFrom(InputStream& stream) {
    switch (stream.readInt()) {
    case 0:
        return std::shared_ptr<PlayerMessageGame::CustomDataMessage>(new PlayerMessageGame::CustomDataMessage(PlayerMessageGame::CustomDataMessage::readFrom(stream)));
    case 1:
        return std::shared_ptr<PlayerMessageGame::ActionMessage>(new PlayerMessageGame::ActionMessage(PlayerMessageGame::ActionMessage::readFrom(stream)));
    default:
        throw std::runtime_error("Unexpected discriminant value");
    }
};
