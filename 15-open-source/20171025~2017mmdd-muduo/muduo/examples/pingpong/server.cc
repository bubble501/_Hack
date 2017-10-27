//muduo服务端的头文件
#include <muduo/net/TcpServer.h>
//原子（线程安全）
#include <muduo/base/Atomic.h>
//日志模块
#include <muduo/base/Logging.h>
//线程模块
#include <muduo/base/Thread.h>
//EventLoop
#include <muduo/net/EventLoop.h>
#include <muduo/net/InetAddress.h>

#include <boost/bind.hpp>

#include <utility>

#include <stdio.h>
#include <unistd.h>

using namespace muduo;
using namespace muduo::net;

/*
 * 服务端收到连接的回调函数
 */
void onConnection(const TcpConnectionPtr& conn)
{
  if (conn->connected())
  {
    //setTcpNoDelay()函数是什么作用？
    conn->setTcpNoDelay(true);
  }
}

/*
 * 服务端收到消息的回调函数
 * 因为是pingpong测试，功能上是将收到的信息立马发回去
 */
void onMessage(const TcpConnectionPtr& conn, Buffer* buf, Timestamp)
{
  conn->send(buf);
}

int main(int argc, char* argv[])
{
  if (argc < 4)
  {
    fprintf(stderr, "Usage: server <address> <port> <threads>\n");
  }
  else
  {
    LOG_INFO << "pid = " << getpid() << ", tid = " << CurrentThread::tid();
    //设置日志级别
    Logger::setLogLevel(Logger::WARN);

    //获取命令行传入的IP地址信息
    const char* ip = argv[1];
    //获取命令行传入的端口信息
    uint16_t port = static_cast<uint16_t>(atoi(argv[2]));
    //根据IP、端口创建InetAddress对象
    InetAddress listenAddr(ip, port);
    //获取命令行传入的线程数信息
    int threadCount = atoi(argv[3]);

    //EventLoop循环
    EventLoop loop;

    //创建TcpServer对象
    TcpServer server(&loop, listenAddr, "PingPong");

    //设置收到连接的回调函数
    server.setConnectionCallback(onConnection);
    //设置收到信息的回调函数
    server.setMessageCallback(onMessage);

    //设置服务端线程数
    if (threadCount > 1)
    {
      server.setThreadNum(threadCount);
    }

    //启动服务端
    server.start();

    //开始EventLoop循环
    loop.loop();
  }
}
