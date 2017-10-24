关于网络编程的诸多细节整理在此

## 研究方法

使用WireShark、tcpdump工具抓包分析网络包的每个字节，就像使用WinDbg、GDB分析进程内存的每个细节一样

用各种网络IO模型，如阻塞、非阻塞、select、epoll、IOCP等开发网络程序，分别在各个关键节点处用WireShark、tcpdump抓包分析各个关键节点的网络包细节：

* connect
* accept
* send
* close

首先是通过抓包工具、专业数据对TCP/IP通信的细节进行研究，其次是对网络编程的各种IO模型进行总结

另外有必要考虑到网络编程中可能会遇到哪些异常情况，以及应该如何处理：

* 当socket句柄用完了，但还持续有新的连接进来怎么办？
* 多线程编程下服务端线程出现死锁导致大量的请求积压怎么办？
* 请求数量过多，压力太大，导致内存暴涨怎么办？
* 客户端和服务端正在通信的过程中突然断网了怎么办？
* 数据过大，导致内核接收缓冲区和发送缓冲区堆积，内部的数据发不出去怎么办？
* 网络环境差，网速不稳定的情况如何应对？
* 怎么进行网络攻击？遇到网络攻击怎么预防？
* 很多人说关闭连接是最困难的部分，那么如何正确的关闭连接
	* 假如客户端不是恶意的，怎么保证在双方都完整收到数据后再合理关闭？
	* 假如客户端就是恶意占用连接，服务端要怎么办？

还有针对网络编程的诸多问题

* 像libevent、muduo是怎么对各种不同的IO demultiplex进行封装的
* muduo中线程同步的详细细节
* muduo中内存管理的详细细节
* libevent、muduo等网络库中应用数据结构的细节
* web开发中用到的很多东西都是封装好的，是在应用层而非传输层的开发，这时候需要注意什么
* 如何应用网络系统构建分布式系统
* 如何设计高效的网络协议

## 核心的图

关于网络有几张必须牢记于心的核心的图片，在分析网络传输、网络编程的时候必须实时想着这些图。要求就是烂熟于心

>比如神经科医生王拥军，最开始的时候就是连续一年一直在重复性的画脑神经的图，重复到将这些基础的东西印到自己脑子里，训练到让不会再受基础、工具的限制，这些基础、工具都是潜意思的拿来就用的东西，大脑就可以完全用于进行思考本身

**IP协议的报文**

![image](./image/01.png)

**UDP协议的报文**

![image](./image/02.png)

**TCP协议的报文**

![image](./image/03.png)

**TCP与socket对应关系**

![image](./image/04.png)

**TCP状态机**

![image](./image/05.png)

## 文章列表

* [《Socket API到具体的网络包》](https://github.com/HackerLaboratory/_Crack/blob/master/20170821~2017mmdd-network-program-detail/01-tcp-connect-send-close.md)
* [《TCP状态机分析》](https://github.com/HackerLaboratory/_Crack/blob/master/20170821~2017mmdd-network-program-detail/02-tcp-state-machine.md)
* [《TCP中SYN、ACK、URG、PSH、RST、FIN标志位》]()
* [《TCP的Keep-Alive机制》]()
* [《TCP如何保证数据完整性》]()

## 我之前对于网络和网络编程的学习

* [《Linux网络管理1---（Linux配置IP地址，ifconfig、配置文件）》](http://www.xumenger.com/linux-network-20150516-01/)
* [《Linux网络管理2---（网络环境查看命令、网络测试命令）》](http://www.xumenger.com/linux-network-20150516-02/)
* [《Linux按照CPU、内存、磁盘IO、网络性能监测》](http://www.xumenger.com/linux-mem-cpu-net-io-20160629/)
* [《使用WireShark分析TCP的三次握手过程》](http://www.xumenger.com/wireshark-tcp-20160716/)
* [《Delphi网络编程：使用IdTcpServer/IdTcpClient》](http://www.xumenger.com/windows-delphi-socket-20160929/)
* [《Delphi网络编程：使用ServerSocket/ClientSocket》](http://www.xumenger.com/windows-delphi-socket-20161010/)
* [《Delphi网络编程：阻塞和非阻塞》](http://www.xumenger.com/windows-delphi-socket-20161011/)
* [《TCP/IP学习笔记：网络协议的层结构》](http://www.xumenger.com/network-1-20161021/)
* [《TCP/IP学习笔记：初识TCP协议》](http://www.xumenger.com/network-2-20161023/)
* [《Delphi网络编程：FIX网络协议》](http://www.xumenger.com/delphi-network-fix-20161221/)
* [《Delphi网络编程：发送和接收二进制数据》](http://www.xumenger.com/delphi-binary-socket-20161222/)
* [《Delphi网络编程：ServerSocket/ClientSocket的线程分布》](http://www.xumenger.com/01-delphi-socket-thread-20170103/)
* [《Delphi网络编程：ScktComp源码解析初步》](http://www.xumenger.com/02-delphi-socket-source-20170103/)
* [《让 CPU 告诉你硬盘和网络到底有多慢》](http://www.xumenger.com/cpu-mem-disk-network-20170110/)
* [《Python的Requests库：初识Requests库》](http://www.xumenger.com/python-request-01-20170114/)
* [《Python的Requests库：HTTP请求和响应》](http://www.xumenger.com/python-request-02-20170114/)
* [《网络编程第一篇》](http://www.xumenger.com/tcp-ip-program-20170117/)
* [《WireShark抓包分析简单网络问题》](http://www.xumenger.com/tcp-wireshark-20170215/)
* [《ScktComp OnRead的回调逻辑》](http://www.xumenger.com/scktcomp-onread-test-20170329/)
* [《ScktComp非阻塞网络编程的坑》](http://www.xumenger.com/socketapi-error-usage-20170404/)
* [《ScktComp OnRead回调的各种诡异现象》](http://www.xumenger.com/socketapi-onread-20170406/)

在[_Crack](https://github.com/HackerLaboratory/_Crack)中总结的文章

* [network-socket-api](https://github.com/HackerLaboratory/_Crack/tree/master/20170312~20170402-network-socket-api)
* [tcp-detail](https://github.com/HackerLaboratory/_Crack/tree/master/20170317-tcp-detail)
* [network-program](https://github.com/HackerLaboratory/_Crack/tree/master/20170402~20170404-network-program)
* [tcp-detail](https://github.com/HackerLaboratory/_Crack/tree/master/20170405-tcp-detail)
* [network-program](https://github.com/HackerLaboratory/_Crack/tree/master/20170421~20170424-network-program)
* [linux-windows-wireshark-tcpdump-telnet](https://github.com/HackerLaboratory/_Crack/tree/master/20170629-linux-windows-wireshark-tcpdump-telnet)

## 参考资料

* 《TCP/IP详解》
* [Request for Comments (RFC)](https://www.ietf.org/rfc.html)
* [RFC文档目录](http://man.chinaunix.net/develop/rfc/default.htm)
* [如何把计算机组成原理、操作系统、数据结构和计算机网络融会贯通，相互联系起来？](https://www.zhihu.com/question/22017267)
* [《Wireshark抓包工具--TCP数据包seq ack等解读》](http://blog.csdn.net/wang7dao/article/details/16805337)
