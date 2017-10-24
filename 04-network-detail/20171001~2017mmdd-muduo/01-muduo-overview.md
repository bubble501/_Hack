2010年3月我写了一篇《学之者生，用之者死——ACE历史与简评》，其中提到“我心目中理想的网络库”的样子：

* 线程安全，原生支持多核多线程
* 不考虑可移植性，不跨平台，只支持Linux，不支持Windows
* 主要支持x86-64，兼容IA32
* 不支持UDP，只支持TCP
* 不考虑IPv6，只支持IPv4
* 不考虑广域网应用，只考虑局域网（实际上muduo也可以用在广域网上）
* 不考虑公网，只考虑内网。不为安全性做特别的增强
* 只支持一种使用模式：non-blocking IO + one event loop per thread，不支持阻塞IO
*  API简单易用，只暴露具体类和标准库中的类。API不使用non-trivial templates，也不使用虚函数
* 只满足常用需求的90%，不面面俱到，必要的时候以app来适用lib
* 只做library，不做成framework
* 争取全部代码在5000行以内（不含测试），目前muduo网络部分的核心代码约4400行
* 在不增加复杂度的前提下可以支持FreeBSD/Darwin，方便将来用Mac作为开发用机，但不为它做性能优化。也就是说IO multiplexing使用poll(2)和epoll(4)
* 以上条件都满足时，可以考虑搭配Google Protocol Buffers RPC

## 在自己的程序中使用muduo

muduo是静态链接的C++程序库，使用muduo的时候，只需要设置好头文件路径（例如：../build/debug-install/include）和库文件路径（例如../build/debug-install/lib）并链接相应的静态库文件（-lmuduo_net -lmuduo_base）即可

## 目录结构

```
muduo
|---build.sh
|---ChangeLog
|---CMakeLists.txt
|---License
|---README
|---muduo            muduo库的主体
|   |---base         与网络无关的基础代码，位于::muduo namespace，包括线程库
|   \---net          网络库，位于::muduo::net namespace
|       |--poller    poll(2)和epoll(4)两种IO multiplexing后端
|       |--http      一个简单的可嵌入的web服务器
|       |--inspect   基于以上web服务器的“窥探器”，用于报告进程的状态
|       \--protorpc  简单实现Google Protobuf RPC，不推荐使用
|---examples         丰富的示例
\--TODO
```

muduo的源代码文件名与class名相同，例如ThreadPool class的定义是muduo/base/ThreadPool.h，其实现位于muduo/base/ThreadPool.cc

**基础库**

muduo/base目录是一些基础库，都是用户可见的类，内容包括

```
muduo
\--base
   |---AsyncLogging.{h,cc}     异步日志backend
   |---Atomic.h                原子操作和原子整数
```