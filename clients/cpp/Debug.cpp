#include "Debug.hpp"
#include "model/PlayerMessageGame.hpp"

Debug::Debug(const std::shared_ptr<OutputStream> &outputStream)
    : outputStream(outputStream) {}

void Debug::draw(const CustomData &customData) {
  outputStream->write(PlayerMessageGame::CustomDataMessage::TAG);
  customData.writeTo(*outputStream);
  outputStream->flush();
}