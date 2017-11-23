一直都对网络编程感兴趣，毕竟网络在我们的生活中有如此广泛的应用。所以我总是希望自己能够弄清楚网络的工作原理。加上自己在平时的工作中也很多的接触网络编程，但是自己一直没有形成一套完成的网络编程标准

其实网络编程也是有固定的套路的，比如像libevent、muduo等网络库中都是使用一些成熟的网络编程模式来实现的，最常见的就是Reactor模式；相对的，Proactor模式目前在网络库的开发中使用的其实还是比较少的

这个过程我会参考众多资料：

* 网络上众多的研究muduo源码的文章
* 陈硕的书《Linux多线程服务端编程》
* 等等

我会转抄众多的资料，也会自己跟着书上的实验自己逐步完成，在实践中深刻体会网络原理细节、网络编程细节！

我会在这个过程中尝试输出以下内容：

* 阅读muduo源码的过程中，添加详细的注解
* 跟着《Linux多线程服务端编程》将实验认真完成，并且思考和总结背后的原理
* 最终要在[www.xumenger.com](www.xumenger.com)上整理出几篇高质量的关于网络和网络编程的文章
* 熟悉C++的各种特性的用法
* 整理网络编程的套路
	* 如何规范的使用各种Socket API
	* 都说关闭连接是最难的，那应该如何优雅的关闭连接
* 整理内存管理的套路
* 整理多线程编程的套路
	* 如何安全的做到多线程同步
	* 如何保证不因为线程同步过分影响程序性能
* 思考编码的整洁度、规范方面的细节
* 高压下如何保证程序的稳定性

>下面记录我阅读源码的顺序

## (20171026)阅读完examples/pingpong

首先通过这个简单的例子对muduo的API该如何使用有一个认识，为后续研究muduo内部的实现做准备！另外这个例子中有众多C++特性的使用，比如boost的广泛使用。我对于C++的诸多特性使用的还是比较少的，所以这也是一个很好的机会去更深入的了解C++

阅读和注解代码中遇到的问题：

* 对boost::bind的用法进行了梳理
* Channel这个类具体的作用是什么？
* ::socketpair()这个接口是什么作用？
* struct rlimit这个结构体是啥？
* EventLoop的实现细节和工作逻辑细节？

## (20171027)阅读examples/curl

* 对boost::function的用法进行了梳理
* 里面有用到了智能指针boost::shared_ptr
* 另外[《CURL常用命令》](http://www.cnblogs.com/gbyukg/p/3326825.html)可以了解curl
* 目前对于EventLoop、回调的逻辑弄不清楚，目前对调用逻辑不太好理解

## (20171027)阅读muduo/base/Thread

包括的源文件有：Thread.h、Thread.cc、CurrentThread.h

* 用空间换时间的小技巧：
	* 第一次调用获取线程ID的方法时，才进行系统调用获取线程ID
	* 在第一次进行系统调用获取线程ID的同时就将其以int、string分别存储到缓存
	* 后续再获取线程ID的时候可以直接从缓存中获取，而不用进行系统调用！
	* 既用int类型也用string类型是为了在需要任何一种类型的时候不必再去实时转换！
	* 人家对于这么小的点都要做优化，可以好好反思一下自己工作中编写的代码存在多少的性能问题
* \_\_thread关键字类似于Delphi中的threadvar，每个线程有独立的变量实体，互不干扰
	* 可以用这个关键字来定义变量来存储每个线程特定的信息
	* 比如int类型线程ID、string类型线程ID、线程名称等信息
* muduo中的Thread相关的代码主要是对pthread进行了封装！

不过在阅读代码的过程中也发现一些问题：

* pthread\_atfork的详细作用是什么
* std::move()我做了简单整理，但还不是理解的很好
* 为什么把Thread定义为单例的，明明在实际编码中可能会创建多个线程啊？！
	* 还是说boost::noncopyable有其他意义，并不是表示单例？
* muduo中是如何具体使用Thread这个模块的呢？

## (20171101)阅读muduo/base/Log..

包括的源文件有：AsyncLogging.cc、AsyncLogging.h、LogFile.cc、LogFile.h、Logging.cc、Logging.h、LogStream.cc、LogStream.h

* 其实每个源文件中也没有多少代码，其如何封装、如何模块化的思路还是值得学习的
* AsyncLogging封装的是异步写日志线程，内部有一个专门写日志的异步线程
	* 其实异步写日志的实现整体和目前我们报盘组的THsWriteLog实现是一致的
	* 只是可能有一些细节差异！
* LogStream中定义了定长的内存缓冲区、实现了日志流类（主要是重载各种类型参数的 << 操作符）
	* 一次申请定长的内存Buffer，然后重复使用
	* 这个模块中还有涉及到十进制、十六进制转换成字符串的函数实现
	* 重视各种整型：int、long、short之间的转换和兼容关系！

依然存在的一些问题点：

* AsyncLogging模块用到Mutex、Condition、CountDownLatch模块，这些暂时还没有看呢
* 很多C++中STL、boost的用法还是有点糊涂！
* 具体日志模块在muduo中是怎么使用的目前还没有看
* 目前是简单阅读完成，没有介入GDB进行单步调试，分析变量、函数

## (20171101)阅读muduo/base/BlockingQueue

涉及到的源文件：BlockingQueue.h

* 其中用到了MutexLock来进行加锁，保证队列数据的安全性
	* 这个看起来和报盘组内部封装的TMyQueue有些像
* 另外其中用到了Condition
	* 当队列为空的时候，如果去取数据，那么`notEmpty_.wait();`
	* 当往队列中放数据的时候，调用`notEmpty_.notify();`，通知wait的地方继续运行
	* 这个Condition看起来和报盘组内使用的TTaskWakeUp很像

>看到这里，发现很多基础的东西都是互通的（原来很多东西在我当前报盘开发中都已经广泛应用了！），剩下的基础模块可以在后续阅读网络模块的时候再回头看！

## (20171102)阅读muduo/net/Socket

Socket模块是对操作系统层面的socketfd的封装，同时封装并暴露socket API的一些接口，比如bind、listen、accept……

有一些针对TCP原理的问题，比如下面这些TCP参数都分别有什么用？弄清楚这些将会很好的帮助自己理解TCP核心原理

* unrecovered
* rto
* ato
* snd_mss
* rcv_mss
* lost
* retrans
* rtt
* rttvar
* sshthresh
* cwnd
* total_retrans

另外Socket模块提供了设置内核参数的接口，弄清楚这些参数也是理解TCP核心原理的重要途径

* setTcpNoDelay：TCP_NODELAY
* setReuseAddr：SO_REUSEADDR
* setReusePort：SO_REUSEPORT
* setKeepAlive：SO_KEEPALIVE

## (20171102)阅读muduo/net/Channel

Channel这个类不拥有文件描述符（这里说的文件描述符包括：socket、event、timer、signal）

muduo中通过Channel对fd进行封装，其实更合适的说法是对fd事件相关方法的封装，例如负责注册fd的可读或可写事件到EventLoop，又如fd产生事件后要如何响应，一个fd对应一个Channel，它们是聚合关系，Channel在析构函数中并不会close掉这个fd。它有一个handleEvent方法，当该fd有事件产生时EventLoop会调用handleEvent方法进行处理，在handleEvent内部根据可读或可写事件调用不同的回调函数（回调函数可事先注册）。它一般作为其他类的成员，例如EventLoop通过一个vector<Channel\*>对注册到其内的众多fd的管理，毕竟EventLoop就有fd及其对应的事件处理方法，所以EventLoop与Channel是一对多的关系

Socket也是对fd的封装，但不同于Channel，它仅封装::socket产生的fd，并且提供的方法也是一些获取或设置网络连接属性的方法，它和fd是组合关系，当Socket析构时会close掉这个fd。不管如何封装fd，一些系统函数传递的参数总是fd！

>之前阅读过报盘的Binary引擎的代码，看是不是会有存在共通的地方

关于boost::weak\_ptr这个只能指针，可以阅读一下文章[《（推荐）智能指针boost::weak_ptr 详解》](http://blog.csdn.net/acs713/article/details/29175231)。另外自己也在博客中对于boost::shared\_ptr、boost::weak\_ptr这两个智能指针进行了用法上的简单总结

## (20171103)阅读muduo/net/Poller

Poller是IO Multiplexing的实现，是一个抽象类，具体实现由子类PollerPoller（封装poll）、EpollPoller（封装epoll）实现，这是muduo库中唯一一个用面向对象的思想实现的，通过虚函数提供回调功能。Poll中的updateChannel方法用于注册和更新关注的事件，所有的fd都需要调用它添加到事件循环中

除了用TimerQueue和Poller管理时间事件和IO事件外，EventLoop还包含一个任务队列，它用来做一些计算任务，你可以将自己的任务添加到任务队列中，EventLoop在一次事件循环中处理完IO事件就会进行依次取出这些任务进行执行，这样当多个线程需要处理同一资源时可以减少锁的复杂性，将资源的管理固定地交由一个线程来处理，其他线程对资源的处理只需要添加到该线程的任务队列中，由该线程异步执行，如此只需要在任务队列中加锁即可，其他地方无需上锁，减少锁的滥用。但有一个问题，如果EventLoop阻塞在epoll\_wait处，就无法处理这些计算任务了，毕竟计算任务是在处理完IO事件后才执行的，所以此时需要通过某种通信方式唤醒该线程，被唤醒后取出队列中的任务进行执行。muduo采用eventfd(2)来异步唤醒

发现C++中const这个关键字使用的很多，顺便今天对C++中const的用法进行了简单梳理：

* const修饰普通变量
* const修饰指针
* const修饰函数参数
* const修饰函数返回值
* const修饰类成员变量
* const修饰类成员函数

## (20171109)阅读muduo/net/EventLoop..

EventLoop字面意思是事件循环。本模块包含的源码文件有：EventLoop.h、EventLoop.cc、EventLoopThread.h、EventLoopThread.cc、EventLoopThreadPool.h、EventLoopThreadPool.cc

简单总结和介绍一下：

* EventLoop封装的是事件循环
* EventLoopThread封装的是事件循环线程
* EventLoopThreadPool封装的是事件循环线程池，其中使用vector容器来管理多个EventLoopThread

需要继续整理以下内容：

* 在EventLoop.cc源码中，有使用内核调用eventfd
	* 其详细用法、细节还需梳理
	* write、read该怎么配合使用？
* EventLoopThread、EventLoopThreadPool都很简单，都是依赖于EventLoop实现的
* 不过目前，关于EventLoop中调用poll处理事件的细节还没有搞清楚

现在第一遍阅读的感觉是大概懂，但是整体还是一种比较混沌的状态！

Add in 20171123，EventLoop应该是对于事件驱动引擎的封装，最近看的vn.py这个项目中正好也有讲到事件驱动引擎的原理、好处、设计思路，也算是“山重水复疑无路，柳暗花明又一村”的体现，又一次证明底层原理都是相通的！

## (20171110)阅读muduo/net/Timer..

Timer字面意思是定时器。本模块包含的源码文件有：Timer.h、Timer.cc、TimerId.h、TimerQueue.h、TimerQueue.cc，封装定时事件

Timer.h、Timer.cc中主要用到的是Timestamp，所以后续有必要认真看一下Timestamp的代码实现细节！

## (20171116)阅读muduo/net/Acceptor..

该模块是对TCP服务端接收客户端的TCP连接进行的封装！

在Acceptor的构造方法中，代码是这样实现的

```
Acceptor::Acceptor(EventLoop* loop, const InetAddress& listenAddr, bool reuseport)
  : loop_(loop),
    acceptSocket_(sockets::createNonblockingOrDie(listenAddr.family())),
    acceptChannel_(loop, acceptSocket_.fd()),
    listenning_(false),
    idleFd_(::open("/dev/null", O_RDONLY | O_CLOEXEC))
{
  assert(idleFd_ >= 0);
  acceptSocket_.setReuseAddr(true);
  acceptSocket_.setReusePort(reuseport);
  acceptSocket_.bindAddress(listenAddr);
  acceptChannel_.setReadCallback(
      boost::bind(&Acceptor::handleRead, this));
}
```

可以看到这个类是配合Channel、EventLoop使用的，但是目前对于其运行的逻辑还是没有梳理很清楚

## (20171116)阅读muduo/net/TcpConnection..

TcpConnection是对一个TCP连接的封装，**主要**在服务端用到，当收到一个客户端连接后就会创建一个TcpConnection对象来管理TCP连接

其中在调用SocketAPI send/write方法的时候，其对返回值的判断需要特别注意

```
    nwrote = sockets::write(channel_->fd(), data, len);
    if (nwrote >= 0)
    {
      remaining = len - nwrote;
      /*
       * 如果发送完成，并且定义了发送完成的回调函数
       * 那么就回调该回调函数！
       */
      if (remaining == 0 && writeCompleteCallback_)
      {
        //queueInLoop是做什么用的？
        loop_->queueInLoop(boost::bind(writeCompleteCallback_, shared_from_this()));
      }
    }
    else // nwrote < 0
    {
      nwrote = 0;
      if (errno != EWOULDBLOCK)
      {
        LOG_SYSERR << "TcpConnection::sendInLoop";
        if (errno == EPIPE || errno == ECONNRESET) // FIXME: any others?
        {
          faultError = true;
        }
      }
    }
```

另外在TcpConnection中大量使用到了EventLoop::queueInLoop、EventLoop::runAfter、EventLoop::runInLoop这些方法，需要弄清楚这些方法的内部细节逻辑！！

另外TcpConnection中的各种回调逻辑目前也并不是很清晰！

## (20171116)muduo/net/TcpClient..

之前看了TcpConnection这个模块的代码，在TcpClient中有用到TcpConnection，但是还用到了另外一个和连接相关的模块Connector，接下来看一下这个模块的代码实现细节！

## 最后需要总结的一些技术点

* C++语法层面
	* 智能指针的使用
	* STL的使用
	* std::move的作用、原理和用法
	* boost::function
	* boost::shared_ptr
	* boost::scoped_ptr
	* std::deque
	* boost::bind
	* boost的更多使用技巧
* 操作系统层面
	* 多线程
	* 线程同步
* 内存管理层面
	* 如果释放了内存地址，但还有某个地方有指针指向这个地址，如果不小心访问就会出问题！这个该怎么应对？
* 文件IO层面
* Socket API层面
	* 如何优雅地关闭TCP连接
	* 如果旧的TCP关闭了，结果代码中还有地方在用这个socket，新来的连接socket正好和老的socket重复了，那么这种场景如何应对；类似于释放了内存地址，但还有某个地方有指针指向这个地址，如果不小心访问就会出问题！
	* Linux下TCP协议的各种参数、开关的作用！
	* eventfd的用法细节
* 编码规范层面：缩进、命名规范、函数封装、类封装