#include "TcpStream.hpp"
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <string>

#ifdef _WIN32
typedef int RECV_SEND_T;
#else
typedef ssize_t RECV_SEND_T;
#endif

TcpStream::TcpStream(const std::string &host, int port) {
#ifdef _WIN32
  WSADATA wsa_data;
  if (WSAStartup(MAKEWORD(1, 1), &wsa_data) != 0) {
    throw std::runtime_error("Failed to initialize sockets");
  }
#endif
  sock = socket(AF_INET, SOCK_STREAM, 0);
  if (sock == -1) {
    throw std::runtime_error("Failed to create socket");
  }
  int yes = 1;
  if (setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (char *)&yes, sizeof(int)) <
      0) {
    throw std::runtime_error("Failed to set TCP_NODELAY");
  }
  addrinfo hints, *servinfo;
  std::memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  if (getaddrinfo(host.c_str(), std::to_string(port).c_str(), &hints,
                  &servinfo) != 0) {
    throw std::runtime_error("Failed to get addr info");
  }
  if (connect(sock, servinfo->ai_addr, servinfo->ai_addrlen) == -1) {
    throw std::runtime_error("Failed to connect");
  }
  freeaddrinfo(servinfo);
}

class TcpInputStream : public InputStream {
public:
  TcpInputStream(std::shared_ptr<TcpStream> tcpStream)
      : tcpStream(tcpStream), bufferPos(0), bufferSize(0) {}
  void readBytes(char *buffer, size_t byteCount) {
    while (byteCount > 0) {
      if (bufferSize > 0) {
        if (bufferSize >= byteCount) {
          memcpy(buffer, this->buffer + bufferPos, byteCount);
          bufferPos += byteCount;
          bufferSize -= byteCount;
          return;
        }
        memcpy(buffer, this->buffer + bufferPos, bufferSize);
        buffer += bufferSize;
        byteCount -= bufferSize;
        bufferPos += bufferSize;
        bufferSize = 0;
      }
      if (bufferPos == BUFFER_CAPACITY) {
        bufferPos = 0;
      }
      RECV_SEND_T received =
          recv(tcpStream->sock, this->buffer + bufferPos + bufferSize,
               BUFFER_CAPACITY - bufferPos - bufferSize, 0);
      if (received < 0) {
        throw std::runtime_error("Failed to read from socket");
      }
      bufferSize += received;
    }
  }

private:
  static const size_t BUFFER_CAPACITY = 8 * 1024;
  char buffer[BUFFER_CAPACITY];
  size_t bufferPos;
  size_t bufferSize;
  std::shared_ptr<TcpStream> tcpStream;
};

class TcpOutputStream : public OutputStream {
public:
  TcpOutputStream(std::shared_ptr<TcpStream> tcpStream)
      : tcpStream(tcpStream), bufferPos(0), bufferSize(0) {}
  void writeBytes(const char *buffer, size_t byteCount) {
    while (byteCount > 0) {
      size_t capacity = BUFFER_CAPACITY - bufferPos - bufferSize;
      if (capacity >= byteCount) {
        memcpy(this->buffer + bufferPos + bufferSize, buffer, byteCount);
        bufferSize += byteCount;
        return;
      }
      memcpy(this->buffer + bufferPos + bufferSize, buffer, capacity);
      bufferSize += capacity;
      byteCount -= capacity;
      buffer += capacity;
      flush();
    }
  }
  void flush() {
    while (bufferSize > 0) {
      RECV_SEND_T sent =
          send(tcpStream->sock, buffer + bufferPos, bufferSize, 0);
      if (sent < 0) {
        throw std::runtime_error("Failed to write to socket");
      }
      bufferPos += sent;
      bufferSize -= sent;
    }
    bufferPos = 0;
  }

private:
  static const size_t BUFFER_CAPACITY = 8 * 1024;
  char buffer[BUFFER_CAPACITY];
  size_t bufferPos;
  size_t bufferSize;
  std::shared_ptr<TcpStream> tcpStream;
};

std::shared_ptr<InputStream>
getInputStream(std::shared_ptr<TcpStream> tcpStream) {
  return std::shared_ptr<TcpInputStream>(new TcpInputStream(tcpStream));
}

std::shared_ptr<OutputStream>
getOutputStream(std::shared_ptr<TcpStream> tcpStream) {
  return std::shared_ptr<TcpOutputStream>(new TcpOutputStream(tcpStream));
}