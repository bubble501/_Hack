## 前言

Libevent是一个轻量级的开源高性能网络库，使用者众多，研究者更甚，相关文章也不少。写这一系列文章的用意在于，一则分享心得；二则对Libevent代码和设计思想做系统的、更深层次的分析，写出来，也可供后来者参考

## Libevent简介

Libevent有几个显著的亮点

* 事件驱动（event-driven），高性能
* 轻量级，专注于网络，不如ACE那么臃肿庞大
* 源代码相当精炼、易读
* 跨平台，支持Windows、Linux、*BSD和Mac OS
* 支持多种IO多路复用技术，epoll、poll、dev/poll、select和kqueue等
* 支持IO，定时器和信号等事件
* 注册事件优先级

## 源码结构

>详细分析源代码之前，如果能对其代码文件的节本结构有了个大概的认识和分类，对于代码的分析将是大有裨益的

libevent的源代码虽然都是在一层文件夹下面，但是其代码分类还是相当清晰的，主要可分为头文件、内部使用的头文件、辅助功能函数、日志、libevent框架、对系统IO多路复用机制的封装、信号管理、定时事件管理、缓冲区管理、基本数据结构和基于libevent的两个实用库等几个部分，有些部分可能就是一个源文件

* 头文件，主要是event.h，事件宏定义、接口函数声明，主要结构体event的声明
* 内部头文件，xxx-internal.h，内部数据结构和函数，对外不可见，以达到信息隐藏的目的
* libevent框架，event.c，event整体框架的代码实现
* 对系统IO多路复用机制的封装
	* epoll.c：对epoll的封装
	* select.c：对select的封装
	* devpoll.c：对dev/poll的封装
	* kqueue.c：对kqueue的封装
* 定时事件管理，min-heap.h，其实就是一个以时间作为key的小根堆结构
* 信号管理，signal.c，对信号事件的处理
* 辅助功能函数，evutil.h和evutil.c，一些辅助功能函数，包括创建socket pair和一些时间操作函数：加、减和比较等
* 日志，log.h和log.c，log日志函数
* 缓冲区管理，evbuffer.c和buffer.c，libevent对缓冲区的封装
* 基本数据结构，compat\sys下的两个源文件（大量使用宏定义）
	* queue.h是libevent基本数据结构的实现，包括链表、双向链表、队列等
	* \_libevent\_time.h，一些用于时间操作的结构体定义、函数和宏定义
* 实用网络库，http、evdns是基于libevent实现的http服务器和异步dns(域名系统)查询库
