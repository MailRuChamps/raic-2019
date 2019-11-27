#ifndef _STREAM_HPP_
#define _STREAM_HPP_

#include <string>

class InputStream {
public:
  virtual void readBytes(char *buffer, size_t byteCount) = 0;
  bool readBool();
  int readInt();
  long long readLongLong();
  float readFloat();
  double readDouble();
  std::string readString();
};

class OutputStream {
public:
  virtual void writeBytes(const char *buffer, size_t byteCount) = 0;
  virtual void flush() = 0;
  void write(bool value);
  void write(int value);
  void write(long long value);
  void write(float value);
  void write(double value);
  void write(const std::string &value);
};

#endif