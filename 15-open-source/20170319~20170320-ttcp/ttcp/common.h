#pragma once

#include <string>
#include <stdint.h>

struct Options
{
  uint16_t port;
  int length;
  int number;
  bool transmit, receive, nodelay;
  std::string host;
  Options()
    : port(0), length(0), number(0),
      transmit(false), receive(false), nodelay(false)
  {
  }
};

bool parseCommandLine(int argc, char* argv[], Options* opt);
struct sockaddr_in resolveOrDie(const char* host, uint16_t port);

//客户端先发送这个接口体告诉服务端，我接下来将发送number条消息，每个消息的长度是length
struct SessionMessage
{
  int32_t number;
  int32_t length;
} __attribute__ ((__packed__));

//客户端发送给服务端的消息
struct PayloadMessage
{
  int32_t length;
  char data[0];   //C语言的技巧，char data[0]，表示该数组是变长数组
};

void transmit(const Options& opt);

void receive(const Options& opt);
