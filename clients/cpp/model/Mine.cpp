#include "Mine.hpp"

Mine::Mine() { }
Mine::Mine(int playerId, Vec2Double position, Vec2Double size, MineState state, std::shared_ptr<double> timer, double triggerRadius, ExplosionParams explosionParams) : playerId(playerId), position(position), size(size), state(state), timer(timer), triggerRadius(triggerRadius), explosionParams(explosionParams) { }
Mine Mine::readFrom(InputStream& stream) {
    Mine result;
    result.playerId = stream.readInt();
    result.position = Vec2Double::readFrom(stream);
    result.size = Vec2Double::readFrom(stream);
    switch (stream.readInt()) {
    case 0:
        result.state = MineState::PREPARING;
        break;
    case 1:
        result.state = MineState::IDLE;
        break;
    case 2:
        result.state = MineState::TRIGGERED;
        break;
    case 3:
        result.state = MineState::EXPLODED;
        break;
    default:
        throw std::runtime_error("Unexpected discriminant value");
    }
    if (stream.readBool()) {
        result.timer = std::shared_ptr<double>(new double());
        *result.timer = stream.readDouble();
    } else {
        result.timer = std::shared_ptr<double>();
    }
    result.triggerRadius = stream.readDouble();
    result.explosionParams = ExplosionParams::readFrom(stream);
    return result;
}
void Mine::writeTo(OutputStream& stream) const {
    stream.write(playerId);
    position.writeTo(stream);
    size.writeTo(stream);
    stream.write((int)(state));
    if (timer) {
        stream.write(false);
    } else {
        stream.write(true);
        stream.write((*timer));
    }
    stream.write(triggerRadius);
    explosionParams.writeTo(stream);
}
std::string Mine::toString() const {
    return std::string("Mine") + "(" +
        std::to_string(playerId) +
        position.toString() +
        size.toString() +
        "TODO" + 
        "TODO" + 
        std::to_string(triggerRadius) +
        explosionParams.toString() +
        ")";
}
