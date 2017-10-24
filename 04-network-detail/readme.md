最近在深入研究网络编程，这里对于各种常见的网络IO模型进行研究

目前主要是通过在网络上搜索不错的文章进行初步的了解

因为是在网络上找到的文章，所以对其中的内容可信度还是要保持怀疑的

另外自己在平时的开发中也坚持整理了不少关于网络编程的文章：

* [《ScktComp OnRead回调的各种诡异现象》](http://www.xumenger.com/socketapi-onread-20170406/)
* [《ScktComp非阻塞网络编程的坑》](http://www.xumenger.com/socketapi-error-usage-20170404/)
* [《ScktComp OnRead的回调逻辑》](http://www.xumenger.com/scktcomp-onread-test-20170329/)
* [《WireShark抓包分析简单网络问题》](http://www.xumenger.com/tcp-wireshark-20170215/)
* [《Python使用web.py进行简单Web开发》](http://www.xumenger.com/python-webpy-20170115/)
* [《Python的Requests库：HTTP请求和响应》](http://www.xumenger.com/python-request-02-20170114/)
* [《Python的Requests库：初识Requests库》](http://www.xumenger.com/python-request-01-20170114/)
* [《Delphi网络编程：ScktComp源码解析初步》](http://www.xumenger.com/02-delphi-socket-source-20170103/)
* [《Delphi网络编程：ServerSocket/ClientSocket的线程分布》](http://www.xumenger.com/01-delphi-socket-thread-20170103/)
* [《Python网络爬虫概述》](http://www.xumenger.com/python-crawler-20170102/)
* [《Delphi网络编程：发送和接收二进制数据》](http://www.xumenger.com/delphi-binary-socket-20161222/)
* [《Delphi网络编程：FIX网络协议》](http://www.xumenger.com/delphi-network-fix-20161221/)
* [《TCP/IP学习笔记：初识TCP协议》](http://www.xumenger.com/network-2-20161023/)
* [《TCP/IP学习笔记：网络协议的层结构》](http://www.xumenger.com/network-1-20161021/)
* [《使用webbench对tinyhttpd进行压力测试》](http://www.xumenger.com/tinyhttpd-webbench-20161012/)
* [《Delphi网络编程：阻塞和非阻塞》](http://www.xumenger.com/windows-delphi-socket-20161011/)
* [《Delphi网络编程：使用ServerSocket/ClientSocket》](http://www.xumenger.com/windows-delphi-socket-20161010/)
* [《Delphi网络编程：使用IdTcpServer/IdTcpClient》](http://www.xumenger.com/windows-delphi-socket-20160929/)

自己也研究过一些网络编程相关的源码

* [20170208~20170208-tinyhttpd](https://github.com/xumenger/xumenger.github.open/tree/master/20170208~20170208-tinyhttpd)
* [20170210~20170211-webbench](https://github.com/xumenger/xumenger.github.open/tree/master/20170210~20170211-webbench)
* [20170314~201703dd-scktcomp](https://github.com/xumenger/xumenger.github.open/tree/master/20170314~201703dd-scktcomp)

也学习《网络编程》整理过一些笔记

* [20170312~20170402-network-socket-api](https://github.com/xumenger/xumenger.github.crack/tree/master/20170312~20170402-network-socket-api)
* [20170402~20170404-network-program](https://github.com/xumenger/xumenger.github.crack/tree/master/20170402~20170404-network-program)
* [20170317-tcp-detail](https://github.com/xumenger/xumenger.github.crack/tree/master/20170317-tcp-detail)
* [20170317-tcp-detail](https://github.com/xumenger/xumenger.github.crack/tree/master/20170405-tcp-detail)

但是到目前为止，对于网络编程依然存在诸多的疑问

并且在实际的编程中对于一下三个点的应用虽然很多，但是极其不精细，也导致了很多问题

* 规范的多线程编程
* 规范的、严谨的内存管理
* 规范的网络编程
