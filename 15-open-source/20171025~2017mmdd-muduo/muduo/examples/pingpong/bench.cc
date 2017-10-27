// Benchmark inspired by libevent/test/bench.c
// See also: http://libev.schmorp.de/bench.html

#include <muduo/base/Logging.h>
#include <muduo/base/Thread.h>
#include <muduo/net/Channel.h>
#include <muduo/net/EventLoop.h>

#include <boost/bind.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include <stdio.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <unistd.h>

using namespace muduo;
using namespace muduo::net;

/*
 * 下面定义全局变量
 */
std::vector<int> g_pipes;
int numPipes;
int numActive;
int numWrites;
EventLoop* g_loop;
/*
 * muduo中通过Channel对fd进行封装，其实更合适的说法是对fd事件相关方法的封装
 * 例如负责注册fd的可读或可写事件到EventLoop，又如fd产生时间后要如何响应
 * 一个fd对应一个Channel，它们是聚合关系
 * Channel在析构函数中并不会close掉这个fd
 * 它有一个handleEvent方法，当该fd有事件产生时EventLoop会调用handleEvent方法进行处理
 * 在handleEvent内部根据可读或可写时间调用不同的回调函数（回调函数可事先注册）
 * 它一般作为其他类的成员，例如EventLoop通过一个vector<Channel *>对注册到其内的众多fd的管理
 * 毕竟EventLoop就有了fd及其对应的事件处理方法
 */
boost::ptr_vector<Channel> g_channels;

int g_reads, g_writes, g_fired;

//收到数据后，读回调方法
void readCallback(Timestamp, int fd, int idx)
{
  char ch;

  //调用Socket API的recv方法，recv返回实际读到的内容
  //这里面每次读一个字节！
  g_reads += static_cast<int>(::recv(fd, &ch, sizeof(ch), 0));
  if (g_writes > 0)
  {
    int widx = idx+1;
    if (widx >= numPipes)
    {
      widx -= numPipes;
    }
    /* 
     * 发送数据
     * std::vector<int> g_pipes;
     */
    ::send(g_pipes[2 * widx + 1], "m", 1, 0);
    g_writes--;
    g_fired++;
  }
  if (g_fired == g_reads)
  {
    //`EventLoop* g_loop;`，调用quit是什么作用
    g_loop->quit();
  }
}

std::pair<int, int> runOnce()
{
  //获取函数入口时的时间戳
  Timestamp beforeInit(Timestamp::now());
  
  //循环为numPipes个Channel设置read回调函数
  for (int i = 0; i < numPipes; ++i)
  {
    //boost::ptr_vector<Channel> g_channels;
    Channel& channel = g_channels[i];
    //设置读数据的回调函数
    channel.setReadCallback(boost::bind(readCallback, _1, channel.fd(), i));
    //???
    channel.enableReading();
  }

  int space = numPipes / numActive;
  space *= 2;
  for (int i = 0; i < numActive; ++i)
  {
    //std::vector<int> g_pipes;
    ::send(g_pipes[i * space + 1], "m", 1, 0);
  }

  g_fired = numActive;
  g_reads = 0;
  g_writes = numWrites;
  
  //获取EventLoop循环之前的时间戳
  Timestamp beforeLoop(Timestamp::now());
  //开始EventLoop循环
  g_loop->loop();
  //获取EventLoop循环之后的时间戳
  Timestamp end(Timestamp::now());

  int iterTime = static_cast<int>(end.microSecondsSinceEpoch() - beforeInit.microSecondsSinceEpoch()); //函数初始化到结束的时间差
  int loopTime = static_cast<int>(end.microSecondsSinceEpoch() - beforeLoop.microSecondsSinceEpoch()); //EventLoop的时间差
  //返回时间差键值对
  return std::make_pair(iterTime, loopTime);
}

int main(int argc, char* argv[])
{
  numPipes = 100;
  numActive = 1;
  numWrites = 100;
  int c;
  //获取命令行参数信息
  while ((c = getopt(argc, argv, "n:a:w:")) != -1)
  {
    switch (c)
    {
      case 'n':
        numPipes = atoi(optarg);    //Channel个数、socket_pair个数
        break;
      case 'a':
        numActive = atoi(optarg);   //??
        break;
      case 'w':
        numWrites = atoi(optarg);   //??
        break;
      default:
        fprintf(stderr, "Illegal argument \"%c\"\n", c);
        return 1;
    }
  }

  struct rlimit rl;
  rl.rlim_cur = rl.rlim_max = numPipes * 2 + 50;
  if (::setrlimit(RLIMIT_NOFILE, &rl) == -1)
  {
    perror("setrlimit");
    //return 1;  // comment out this line if under valgrind
  }
  
  /*
   * std::vector<int> g_pipes;
   * <<Effective STL>>中讲到，如果清楚vector的大小，那么先resize提前定好大小，省得用的过程中动态变内存！
   */
  g_pipes.resize(2 * numPipes);
  for (int i = 0; i < numPipes; ++i)
  {
    //socketpair看起来向是创建一个socket对，不知道和libevent中的用法是不是一样
    if (::socketpair(AF_UNIX, SOCK_STREAM, 0, &g_pipes[i*2]) == -1)
    {
      perror("pipe");
      return 1;
    }
  }

  EventLoop loop;
  g_loop = &loop;

  //创建numPipes个Channel，并放到向量vector中
  for (int i = 0; i < numPipes; ++i)
  {
    Channel* channel = new Channel(&loop, g_pipes[i*2]);
    g_channels.push_back(channel);
  }

  //这是什么鬼东西？
  for (int i = 0; i < 25; ++i)
  {
    std::pair<int, int> t = runOnce();
    printf("%8d %8d\n", t.first, t.second);
  }

  /*
   * boost::ptr_vector<Channel> g_channels;
   * 这里注意STL、boost、智能指针的用法细节
   */
  for (boost::ptr_vector<Channel>::iterator it = g_channels.begin();
       it != g_channels.end(); ++it)
  {
    it->disableAll();
    it->remove();
  }
  g_channels.clear();
}

