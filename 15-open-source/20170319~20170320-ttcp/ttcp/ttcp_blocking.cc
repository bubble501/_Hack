#include <examples/ace/ttcp/common.h>
#include <muduo/base/Timestamp.h>

#undef NDEBUG

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <strings.h>

#include <netinet/in.h>
#include <arpa/inet.h>

//该函数用于在服务端接收新的TCP连接
static int acceptOrDie(uint16_t port)
{
  //创建服务端Socket
  int listenfd = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  assert(listenfd >= 0);

  int yes = 1;
  //设置Socket配置
  if (setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes)))
  {
    perror("setsockopt");
    exit(1);
  }

  struct sockaddr_in addr;
  bzero(&addr, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = INADDR_ANY;
  //绑定端口、IP
  if (bind(listenfd, reinterpret_cast<struct sockaddr*>(&addr), sizeof(addr)))
  {
    perror("bind");
    exit(1);
  }

  //监听服务端的端口
  if (listen(listenfd, 5))
  {
    perror("listen");
    exit(1);
  }

  struct sockaddr_in peer_addr;
  bzero(&peer_addr, sizeof(peer_addr));
  socklen_t addrlen = 0;

  //接受客户端连接
  int sockfd = ::accept(listenfd, reinterpret_cast<struct sockaddr*>(&peer_addr), &addrlen);
  if (sockfd < 0)
  {
    perror("accept");
    exit(1);
  }
  ::close(listenfd);
  return sockfd;
}

//完整地写n个字节
static int write_n(int sockfd, const void* buf, int length)
{
  int written = 0;
  while (written < length)
  {
    ssize_t nw = ::write(sockfd, static_cast<const char*>(buf) + written, length - written);
    if (nw > 0)
    {
      written += static_cast<int>(nw);
    }
    else if (nw == 0)
    {
      break;  // EOF
    }
    else if (errno != EINTR)
    {
      perror("write");
      break;
    }
  }
  return written;
}

//完整地读n个字节
static int read_n(int sockfd, void* buf, int length)
{
  int nread = 0;
  while (nread < length)
  {
    ssize_t nr = ::read(sockfd, static_cast<char*>(buf) + nread, length - nread);
    if (nr > 0)
    {
      nread += static_cast<int>(nr);
    }
    else if (nr == 0)
    {
      break;  // EOF
    }
    else if (errno != EINTR)
    {
      perror("read");
      break;
    }
  }
  return nread;
}

//transmit，发送函数
void transmit(const Options& opt)
{
  struct sockaddr_in addr = resolveOrDie(opt.host.c_str(), opt.port);
  printf("connecting to %s:%d\n", inet_ntoa(addr.sin_addr), opt.port);

  int sockfd = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  assert(sockfd >= 0);
  int ret = ::connect(sockfd, reinterpret_cast<struct sockaddr*>(&addr), sizeof(addr));
  if (ret)
  {
    perror("connect");
    printf("Unable to connect %s\n", opt.host.c_str());
    ::close(sockfd);
    return;
  }

  printf("connected\n");
  muduo::Timestamp start(muduo::Timestamp::now());
  struct SessionMessage sessionMessage = { 0, 0 };
  sessionMessage.number = htonl(opt.number);
  sessionMessage.length = htonl(opt.length);
  if (write_n(sockfd, &sessionMessage, sizeof(sessionMessage)) != sizeof(sessionMessage))
  {
    perror("write SessionMessage");
    exit(1);
  }

  const int total_len = static_cast<int>(sizeof(int32_t) + opt.length);
  PayloadMessage* payload = static_cast<PayloadMessage*>(::malloc(total_len));
  assert(payload);
  payload->length = htonl(opt.length);
  for (int i = 0; i < opt.length; ++i)
  {
    payload->data[i] = "0123456789ABCDEF"[i % 16];
  }

  double total_mb = 1.0 * opt.length * opt.number / 1024 / 1024;
  printf("%.3f MiB in total\n", total_mb);

  for (int i = 0; i < opt.number; ++i)
  {
    int nw = write_n(sockfd, payload, total_len);
    assert(nw == total_len);

    int ack = 0;
    int nr = read_n(sockfd, &ack, sizeof(ack));
    assert(nr == sizeof(ack));
    ack = ntohl(ack);
    assert(ack == opt.length);
  }

  ::free(payload);
  ::close(sockfd);
  double elapsed = timeDifference(muduo::Timestamp::now(), start);
  printf("%.3f seconds\n%.3f MiB/s\n", elapsed, total_mb / elapsed);
}

//receive，接收函数，也就是服务端的函数
void receive(const Options& opt)
{
  //先接受一个TCP连接
  int sockfd = acceptOrDie(opt.port);

  //客户端会发送一个SessionMessage消息（该结构体定义在common.h），所以先定义这个变量
  struct SessionMessage sessionMessage = { 0, 0 };
  //读取客户端发送过来的SessionMessage数据，可以看到直接可以发送和接收二进制数据
  if (read_n(sockfd, &sessionMessage, sizeof(sessionMessage)) != sizeof(sessionMessage))
  {
    perror("read SessionMessage");
    exit(1);
  }

  //网络字节序转成本机字节序
  sessionMessage.number = ntohl(sessionMessage.number);
  sessionMessage.length = ntohl(sessionMessage.length);
  //服务端打印：预计接收多少条数据、预计每条数据有多大
  printf("receive number = %d\nreceive length = %d\n",
         sessionMessage.number, sessionMessage.length);

  //准备一块缓冲区来接收数据
  const int total_len = static_cast<int>(sizeof(int32_t) + sessionMessage.length);
  //这里分配内存缓冲区有一个漏洞，假如故意传过来一个很大的sessionMessage.length，会导致这里分配一个很大的内存
  //有可能会导致拒绝响应攻击，所以为了安全可以在分配之前先对其值大小进行判断
  //这里有一个不定长数组语法的应用
  PayloadMessage* payload = static_cast<PayloadMessage*>(::malloc(total_len));
  assert(payload);

  //循环number次读取客户端发送过来的消息
  for (int i = 0; i < sessionMessage.number; ++i)
  {
    payload->length = 0;
    //先读取4个字节，获取每条消息的长度
    if (read_n(sockfd, &payload->length, sizeof(payload->length)) != sizeof(payload->length))
    {
      perror("read length");
      exit(1);
    }
    //网络序转成本机序
    payload->length = ntohl(payload->length);
    //判断SessionMessage中长度与PayloadMessage的长度是否一致
    assert(payload->length == sessionMessage.length);
    if (read_n(sockfd, payload->data, payload->length) != payload->length)
    {
      perror("read payload data");
      exit(1);
    }
    int32_t ack = htonl(payload->length);

    //发送应答给客户端
    if (write_n(sockfd, &ack, sizeof(ack)) != sizeof(ack))
    {
      perror("write ack");
      exit(1);
    }
  }

  //释放申请的内存缓冲区
  ::free(payload);
  //关闭Socket连接
  ::close(sockfd);
}

