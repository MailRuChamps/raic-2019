#ifndef _DEBUG_HPP_
#define _DEBUG_HPP_

#include "Stream.hpp"
#include "model/CustomData.hpp"
#include <memory>

class Debug {
public:
  Debug(const std::shared_ptr<OutputStream> &outputStream);
  void draw(const CustomData &customData);

private:
  std::shared_ptr<OutputStream> outputStream;
};

#endif