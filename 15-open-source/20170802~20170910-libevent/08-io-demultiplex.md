Libevent的核心是事件驱动、同步非阻塞，为了达到这一目标，必须采用系统提供的IO多路复用技术，而这些在Windows、Linux、Unix等不同平台上却各不相同，如何能提供优雅而统一的支持方式？是首要关键的问题，这其实不难，本节就来分析一下

## 统一的关键

Libevent支持多种IO多路复用技术的关键就在于结构体eventop，这个结构体前面也曾提到过，它的成员是一系列的函数指针，定义在event-internal.h文件中

```
struct eventop{
    const char *name;
    void *(*init)(struct event_base *);   //初始化
    int (*add)(void *, struct event *);   //注册事件
    int (*del)(void *, struct event *);   //删除事件
    int (*dispatch)(struct event_base *, void *, struct timeval *);    //事件分发
    void (*dealloc)(struct event_base *, void *);   //注销，释放资源

    /*set if we need to reinitialize the event base*/
    int need_reinit;
};
```

在libevent中，每种IO demultiplex机制的实现必须提供这5个函数接口，来完成自身的初始化、销毁释放；对象注册、注销和分发

比如对于epoll，libevent实现了5个对应的接口函数，并在初始化时将eventop的5个函数指针指向这5个函数，那么程序就可以使用epoll作为IO demultiplex机制了


## 设置IO demultiplex机制

libevent把所有支持的IO demultiplex机制存储在一个全局静态数组eventops中，并在初始化时选择使用何种机制，数组内容根据优先级顺序声明如下：

```
/*In order of preference*/
static const struct eventop * eventops[] = {
#ifdef HAVE_EVENT_PORTS
    &evportops,
#endif
#ifdef HAVE_WORKING_KQUEUE
    &kqops,
#endif
#ifdef HAVE_EPOLL
    &epollops,
#endif
#ifdef HAVE_DEVPOLL
    &devpollops,
#endif
#ifdef HAVE_SELECT
    &selectops,
#endif
#ifdef WIN32
    &win32ops,
#endif
    NULL
};
```

然后libevent根据系统配置和编译选项决定使用哪一种IO demultiplex机制，这段代码在函数event\_base\_new()中

```
base->evbase = NULL;
for(i=0; eventops[i] && !base->evbase; i++){
    base->evel = eventops[i];
    base->evbase = base->evsel->init(base);
}
```

可以看出，libevent在编译阶段选择系统的IO demultiplex机制，而不支持在运行阶段根据配置再次选择

以Linux下面的epoll为例，实现在源文件epoll.c中，eventops对象epollops定义如下

```
const struct eventop epollops = {
    "epoll",
    epoll_init,
    epoll_add,
    epoll_del,
    epoll_dispatch,
    epoll_dealloc,
    1  /* need reinit*/
};
```

变量epollops中的函数指针具体声明如下，注意到期返回值和参数都和eventop中的定义严格一致，这是函数指针的语法限制

```
static void *epoll_init (struct event_base *);
static int epoll_add (void *, struct event *);
static int epoll_del (void *, struct event *);
static int epoll_dispatch(struct event_base *, void *, struct timeval *);
static void epoll_dealloc (struct event_base *, void *);
```

那么如果选择的是epoll，那么调整结构体eventop的init和dispatch函数指针时，实际调用的函数就是epoll的初始化函数epoll\_init()和事件分发函数epoll\_dispatch()了

关于 epoll 的具体用法这里就不多说了，可以参见介绍 epoll 的文章（本人的哈哈）：

http://blog.csdn.net/sparkliang/archive/2009/11/05/4770655.aspx
C++语言提供了虚函数来实现多态，在 C 语言中，这是通过函数指针实现的。对于各类函数指针的详细说明可以参见文章：

http://blog.csdn.net/sparkliang/archive/2009/06/09/4254115.aspx

同样的，上面 epollops 以及 epoll 的各种函数都直接定义在了 epoll.c 源文件中，对外都是不可见的。对于 libevent 的使用者而言，完全不会知道它们的存在，对 epoll 的使用也是通过 eventop 来完成的，达到了信息隐藏的目的

## 小节

支持多种IO demultiplex机制的方法其实挺简单的，借助于函数指针就OK了。通过对源代码的分析也可以看出，libevent是在编译阶段选择系统的IO demultiplex机制的，而不支持在运行阶段根据配置再次选择
