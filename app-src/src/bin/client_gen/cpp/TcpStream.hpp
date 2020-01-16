#ifndef _TCP_STREAM_HPP_
#define _TCP_STREAM_HPP_

#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif
#include <Ws2tcpip.h>
#include <winsock2.h>
#else
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <unistd.h>
typedef int SOCKET;
#endif

#include "Stream.hpp"
#include <memory>
#include <string>

class TcpStream {
public:
  TcpStream(const std::string &host, int port);
  SOCKET sock;
};

std::shared_ptr<InputStream>
getInputStream(std::shared_ptr<TcpStream> tcpStream);

std::shared_ptr<OutputStream>
getOutputStream(std::shared_ptr<TcpStream> tcpStream);

#endif