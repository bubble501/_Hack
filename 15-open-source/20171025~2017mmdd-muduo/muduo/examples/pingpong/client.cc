//muduo客户端的头文件
#include <muduo/net/TcpClient.h>
//muduo封装的日志接口
#include <muduo/base/Logging.h>
//muduo封装的线程
#include <muduo/base/Thread.h>
//事件循环
#include <muduo/net/EventLoop.h>
//??
#include <muduo/net/EventLoopThreadPool.h>
//??
#include <muduo/net/InetAddress.h>
#include <boost/bind.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include <utility>

#include <stdio.h>
#include <unistd.h>

using namespace muduo;
using namespace muduo::net;

class Client;

/*
 * boost::noncopyable比较简单，主要用于单例的情况
 * 通常，写一个单例类就要在类的声明中把它的构造、赋值、函数，复制构造函数隐藏到private或protected中，每个类都这么麻烦
 * 有了noncopyable类，只要让单例类直接继承noncopyable就可以了
 * noncopyable的基本思想是把构造函数和析构函数设置protected权限，这样类可以调用，但外面的类不能调用
 * 当子类需要定义构造函数的时候不至于通不过编译，但是最关键的是noncopyable把复制构造函数和复制赋值函数做出private
 * 这就意味着除非子类定义自己的copy和赋值函数
 * 否则在子类没有定义的情况下，外面的调用者是不能通过赋值和copy构造等手段来产生一个新的子类对象的
 */
class Session : boost::noncopyable
{
 public:
  //构造方法
  Session(EventLoop* loop,                  //EventLoop
          const InetAddress& serverAddr,    //服务端地址信息
          const string& name,               //Session名字
          Client* owner)                    //Client对象
    : client_(loop, serverAddr, name),
      owner_(owner),
      bytesRead_(0),
      bytesWritten_(0),
      messagesRead_(0)
  {
    //设置连接成功的回调函数
    client_.setConnectionCallback(
        /*
         * boost::bind可以将你的方法适配成任何其他方法
         * 其实这得益于C++的模板以及操作符重载
         * 去看boost::bind的实现就会发现它是一个有n多重载的函数，这些重载主要是为了使适应函数的参数个数
         * boost::bind的原理就是函数对象，而函数对象就是重载了()的对象
         
         一般来说boost::bind有两种方式的调用，一种是对自由方法，也取非类方法， 一种是对类方法
           对自由方法来说，直接boost::bind(函数名, 参数1，参数2，...)
           对类方法来说，直接boost::bind(&类名::方法名，类实例指针，参数1，参数2）
           
         举个例子，比如函数`void test(int a, int  b, int c)`
           boost::bind(test, 1, _1, _2)得到一个函数对象b，当我们调用b(3,4)时，相当于调用test(1, 3, 4)
           boost::bind(test, _2, 3, _1)得到一个函数对象b，当我们调用b(3,4)时，相当于调用test(4,3,3)
         
         看明白没有？实际上可以指定一些常量和一些占位符进去，_x这样的就是占位符
           _1表示是既调用时参数的位置，也即b(3, 4)时，_1代表3，_2代表4
         */
        boost::bind(&Session::onConnection, this, _1));
    //设置收到消息的回调函数
    client_.setMessageCallback(
        boost::bind(&Session::onMessage, this, _1, _2, _3));
  }

  //start中，客户端发起连接
  void start()
  {
    client_.connect();
  }

  //stop中，客户端关闭连接
  void stop()
  {
    client_.disconnect();
  }

  //??
  int64_t bytesRead() const
  {
     return bytesRead_;
  }

  //??
  int64_t messagesRead() const
  {
     return messagesRead_;
  }

 private:

  //连接成功后的回调方法
  void onConnection(const TcpConnectionPtr& conn);

  //收到消息后的回调函数
  void onMessage(const TcpConnectionPtr& conn, Buffer* buf, Timestamp)
  {
    //OnMessage的回调次数
    ++messagesRead_;
    //通过buf->readableBytes()获取缓冲区中可读的字节数
    bytesRead_ += buf->readableBytes();    
    //?? bytesRead_和byteWritten_这两个变量有什么区别？
    bytesWritten_ += buf->readableBytes(); 
    //收到的数据放在buf中，然后将收到的数据发出去
    conn->send(buf);   
    
    /*
     * 这个用法是针对这个例子的特例
     * 这个example是做一个pingpong测试，也就是收到服务端的数据，原样发回去
     * 所以这里面bytesRead_、bytesWritten_、conn->send(buf);的用法是这样的
     */
  }

  //Session类中有一个TcpClient成员变量
  TcpClient client_;
  //Session是属于一个Client类对象
  Client* owner_;
  int64_t bytesRead_;
  int64_t bytesWritten_;
  int64_t messagesRead_;
};

/*
 * Client也要一个单例类
 */
class Client : boost::noncopyable
{
 public:
  Client(EventLoop* loop,
         const InetAddress& serverAddr,
         int blockSize,                 //内存块大小
         int sessionCount,              //指定会话个数
         int timeout,
         int threadCount)
    : loop_(loop),
      threadPool_(loop, "pingpong-client"),
      sessionCount_(sessionCount),
      timeout_(timeout)
  {
    //EventLoop的runAfter()是什么作用？
    //这里是不是设置超时时间、超时发生时的回调函数？
    loop->runAfter(timeout, boost::bind(&Client::handleTimeout, this));
    if (threadCount > 1)
    {
      //设置线程池中的线程个数
      threadPool_.setThreadNum(threadCount);
    }
    //是不是启动线程中的所有线程？
    threadPool_.start();

    //设置message_的大小为blockSize值
    for (int i = 0; i < blockSize; ++i)
    {
      // string message_;
      //(i % 128)然后转成char，将char放到message_这个字符串中
      // ? message_存储的是什么内容?
      message_.push_back(static_cast<char>(i % 128));
    }

    //根据构造函数中指定的sessionCount创建指定数量的会话
    for (int i = 0; i < sessionCount; ++i)
    {
      char buf[32];
      snprintf(buf, sizeof buf, "C%05d", i);
      //创建新的会话                 EventLoop                  InetAddress
      Session* session = new Session(threadPool_.getNextLoop(), serverAddr, buf, this);
      //启动会话
      session->start();
      //boost::ptr_vector<Session> sessions_;
      sessions_.push_back(session);
    }
  }

  const string& message() const
  {
    return message_;
  }

  //连接成功的回调函数
  void onConnect()
  {
    //如果连接的个数等于设置的会话的个数
    //那么输出信息：all connected
    if (numConnected_.incrementAndGet() == sessionCount_)
    {
      LOG_WARN << "all connected";
    }
  }

  //断开连接的回调函数
  void onDisconnect(const TcpConnectionPtr& conn)
  {
    if (numConnected_.decrementAndGet() == 0)
    {
      LOG_WARN << "all disconnected";

      int64_t totalBytesRead = 0;     //存储所有会话收到的字节总数
      int64_t totalMessagesRead = 0;  //存储所有会话收到的消息总数
      /*
       * boost::ptr_vector<Class T>定义了一个向量容器
       * 容器中存储的是Class T的智能指针
       * 下面是循环变量所有会话
       */
      for (boost::ptr_vector<Session>::iterator it = sessions_.begin();
          it != sessions_.end(); ++it)
      {
        totalBytesRead += it->bytesRead();
        totalMessagesRead += it->messagesRead();
      }
      //输出信息
      LOG_WARN << totalBytesRead << " total bytes read";
      LOG_WARN << totalMessagesRead << " total messages read";
      LOG_WARN << static_cast<double>(totalBytesRead) / static_cast<double>(totalMessagesRead)
               << " average message size";
      LOG_WARN << static_cast<double>(totalBytesRead) / (timeout_ * 1024 * 1024)
               << " MiB/s throughput";
               
      //下面这行代码又是干什么用的？？
      conn->getLoop()->queueInLoop(boost::bind(&Client::quit, this));
    }
  }

 private:

  void quit()
  {
    loop_->queueInLoop(boost::bind(&EventLoop::quit, loop_));
  }

  //出现超时的时候的回调函数
  void handleTimeout()
  {
    LOG_WARN << "stop";
    /*
     * boost::mem_fn()用于成员函数
     * 将成员函数指针转成函数对象
     */
    std::for_each(sessions_.begin(), sessions_.end(),
                  boost::mem_fn(&Session::stop));
  }

  EventLoop* loop_;
  EventLoopThreadPool threadPool_;
  int sessionCount_;  //会话个数
  int timeout_;       //超时时间
  boost::ptr_vector<Session> sessions_;  //在向量中存储session_信息
  string message_;    //
  AtomicInt32 numConnected_;             //AtomicInt32这个是什么东西？
};

void Session::onConnection(const TcpConnectionPtr& conn)
{
  if (conn->connected())
  {
    conn->setTcpNoDelay(true);
    conn->send(owner_->message());
    owner_->onConnect();
  }
  else
  {
    owner_->onDisconnect(conn);
  }
}

int main(int argc, char* argv[])
{
  if (argc != 7)
  {
    fprintf(stderr, "Usage: client <host_ip> <port> <threads> <blocksize> ");
    fprintf(stderr, "<sessions> <time>\n");
  }
  else
  {
    LOG_INFO << "pid = " << getpid() << ", tid = " << CurrentThread::tid();
    
    //设置日志级别
    Logger::setLogLevel(Logger::WARN);

    //获取命令行中传入的IP信息
    const char* ip = argv[1];
    //获取命令行中传入的Port信息
    uint16_t port = static_cast<uint16_t>(atoi(argv[2]));
    //获取命令行中传入的线程数信息
    int threadCount = atoi(argv[3]);
    //获取命令行中传入的内存块大小
    int blockSize = atoi(argv[4]);
    //获取命令行中传入的会话个数
    int sessionCount = atoi(argv[5]);
    //获取命令行中传入的超时时间
    int timeout = atoi(argv[6]);

    //定义一个EventLoop
    EventLoop loop;
    //传入服务端的IP、端口来生成一个InetAddress临时变量
    InetAddress serverAddr(ip, port);

    //创建一个Client对象
    Client client(&loop, serverAddr, blockSize, sessionCount, timeout, threadCount);
    
    //开始EventLoop循环
    loop.loop();
  }
}

