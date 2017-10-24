>[《[原创]理解I/O Completion Port    nonocast（原作）》](http://dev.gameres.com/Program/Control/IOCP.htm)

欢迎阅读此篇IOCP教程。我将先给出IOCP的定义然后给出它的实现方法，最后剖析一个echo程序来为您拨开IOCP的迷云，出去你心中对IOCP的烦恼。OK，但我不能保证你明白IOCP的一切，但我会尽我最大的努力。以下是我会在这篇文章中提到的相关技术：

* IO端口
* 同步/异步
* 阻塞/非阻塞
* 服务端/客户端
* 多线程程序设计
* Winsock API 2.0

在这之前，我曾经开发过一个项目，其中一块需要网络支持，当时还考虑到了代码的可移植性，只要使用select、connect、accept、listen、send、recv，再加上几个#ifdef的封装以用来处理Winsock和BSD套接字(socket)中间的不兼容性，一个网络子系统只用了几个小时很少的代码就写出来了，至今还让我很回味。那以后很长时间也没有再碰了

前些日子，我们策划做一个网络游戏，我主动承担下网络这一块，想想这还不是小case，心里还偷着乐啊。网络游戏好啊，网络游戏为成百上千的玩家提供了乐趣和令人着迷的游戏体验，他们在线上互相战斗或是加入队伍去战胜共同的敌人。我信心满满的准备开写我的网络，于是乎，发现过去的阻塞同步模式根本不能拿到一个巨量玩家[MMP]的架构中去，直接否定掉了。于是乎，就有了IOCP，如果能够很轻而易举的搞定IOCP，也就不会有这篇教程了。下面请诸位随我进入正题

## 什么是IOCP？

先让我们看看对IOCP的评价

IO完成端口可能是Win32提供的最复杂的内核对象——[Advanced Windows 3rd] Jeffrey Richter

这[IOCP]是实现高容量网络服务器的最佳方法——[Windows Sockets2.0:Write Scalable Winsock Apps Using Completion Ports] 
Microsoft Corporation

完成端口模型提供了最好的伸缩性。这个模型非常适用来处理数百乃至上千个套接字——[Windows网络编程2nd] Anthony Jones & Jim Ohlund

IO Completion Port特别显得重要，因为它们是唯一适用于高负载服务器（必须同时维护许多连接线路）的一个技术。Completion Port利用一些线程，帮助平衡由IO请求所引起的负载。这样的架构特别适合用在SMP系统中产生的“scalable”服务器——[Win32多线程程序设计] Jim Beveridge & Robert Wiener 

## IOCP到底是什么呢？

看来我们完全有理由相信IOCP是大型网络架构的首选，那么IOCP到底是什么呢？

微软在Winsock2中引入IOCP的概念。IOCP全称IO Completion Port，中文译为IOCP完成端口。IOCP是一个异步IO的API，它可以高效地将IO通知给应用程序。与使用select()或是其他异步方法不同的是，一个套接字(socket)与一个完成端口关联了起来，然后就可以继续进行正常的Winsock操作了。然而，当一个事件发生的时候，此完成端口就将被操作系统加入一个队列中，然后应用程序可以对核心层进行查询以得到此完成端口

这里我要对上面的一些概念略作补充，在解释【完成】两字之前，我想先简单的提一下同步和异步这两个概念，逻辑上来讲做完一件事再去做另一件事就是同步，而同时一起做两件或两件以上的事就是异步了。你也可以拿单线程和多线程来作比喻。但是我们一定要将同步和阻塞，异步和非阻塞区分开来，所谓的阻塞函数诸如accept()，当调用此函数后，此时线程将挂起，直到操作系统来通知它，“hey 兄弟，有人连进来了”，那个挂起的线程将继续进行工作，也就符合“生产者—消费者”模型。阻塞和同步看上去有两分相似，但确实完全不同的概念。大家都知道IO设备是一个相对慢速的设备，无论打印机、调制解调器、甚至硬盘，与CPU相比都是奇慢无比的，坐下来等IO的完成是一件不甚明智的事情，有时候数据的流动率非常惊人，把数据从你的文件服务器中以Ethernet速度搬走，其速度可能高达每秒100万字节，如果你尝试从文件服务器中读取100KB，在用户的眼光来看几乎是瞬间完成的，但是要知道，你的线程执行这个命令，已经浪费了10个100万次CPU周期。所以，我们一般使用另一个线程来进行IO。重叠IO（Overlapped IO）是Win32的一项技术，**你可以要求操作系统来为你传送数据，并且在传送完毕时通知你。**这也就是【完成】的含义。这项技术使得你的程序在IO进行过程中仍然能继续处理事务。事实上，操作系统内部正是以线程来完成Overlapped IO。你可以获得线程所有的利益，而不需要付出痛苦的代价

完成端口中所谓的【端口】并不是我们在TCP/IP中所提到的端口，可以说是完全没有关系。我到现在也没有想通一个IO设备（IO Device）和端口（IOCP中的Port）有什么关系。估计这个端口也迷惑了不少人。IOCP只不过是用来进行读写操作，和文件IO倒是有些类似。既然是一个读写设备，我们所能要求它的只是在处理读写上的高效。在文章的第三部分你会轻而易举的发现IOCP设计的真正用意

## IOCP和网络又有什么关系？

```
int main()
{
	WSAStartup(MAKEWORD(2, 2), &wsaData);
	ListeningSocket = socket(AF_INET, SOCK_STREAM, 0);
	bind(ListeningSocket, (SOCKADDR*)$ServerAddr, sizeof(ServerAddr));
	listen(ListeningSocket, 5);
	int nlistenAddrLen = sizeof(ClientAddr);
	while(true){
		NewConnection = accept(ListeningSocket, (SOCKADDR*)&ClientAddr, &nListenAddrLen);
		HANDLE hThread = CreateThread(NULL, 0, ThreadFunc, (void*)NewConnection, 0, &dwThreadId);
		CloseHandle(hThread);
	}
	return 0;
}
```

相信只要写过网络的朋友，应该对这样的结构再熟悉不过了。accept后线程被挂起，等待一个客户发出请求，而后创建新线程来处理请求。当新线程处理客户请求时，起初的线程循环回去等待另一个客户请求。处理客户请求的线程处理完毕后终结

在上述的并发模型中，对每个客户请求都创建了一个线程。其优点在于等待请求的线程只需要做很少的工作。大多数时间中，该线程在休眠（因为recv处于阻塞状态）

但是当并发模型应用在服务器端（基于Windows NT），Windows NT小组注意到这些应用程序的性能没有预料的那么高。特别的，处理很多同时的客户请求意味着很多线程并发地运行在系统中。因为所有这些线程都是可运行的（没有被挂起和等待发生什么事），**MicroSoft意识到NT内核花费了太多的时间来转换运行线程的上下文，线程就没有得到很多CPU时间来做它们的工作**

大家可能也都感觉到并行模型的瓶颈在于它为每个客户请求都创建了一个新线程。创建下啊按成比起出啊昂见进程开销要小，但也远不是没有开销的

我们不妨设想一下：如果事先开好N个线程，让它们在那hold（阻塞），然后可以将所有用户的请求都投递到一个消息队列中去。然后那N个线程逐一从消息队列中去取出消息并加以处理。就可以避免针对每一个用户请求都开线程。不仅减少了线程的资源，也提高了线程的利用率。理论上很不错，你想我等泛泛之辈都能想出来的问题，Microsoft又怎么没有考虑到呢？

这个问题的解决方法就是一个称为IO完成端口的内核对象，它首次在Windows NT 3.5中被引入

其实我们上面的构想应该就差不多是IOCP的设计机理。其实说穿了IOCP不就是一个消息队列吗！你说这和【端口】两字又什么联系呢？我的理解就是IOCP最多是应用程序和操作系统沟通的一个接口罢了

至于IOCP的具体设计那我也很难说得上来，毕竟我没有看过实现的代码，但你完全可以进行模拟，只不过性能可能...，如果想深入理解IOCP，Jeffrey Ritchter的Advanced Windows 3rd其中第13章和第14张有很多宝贵的内容，你可以拿来窥视一下系统是如何完成这一切的。

## 实现方法

MicroSoft为IOCP提供了相应的API函数，主要的就两个，我们逐一的来看一下

```
HANDLE CreateIoCompletionPort(
	HANDLE FileHandle;				//file handle or socket
	HANDLE ExistingCompletionPort;	//handle to IO Completion Port
	ULONG_PTR Completionkey;		//completion key
	DWORD NumberOfConcurrentThreads;//Number of threads to execute concurrently
);
```

在讨论各个参数之前，首先要注意该函数实际用于两个截然不同的目的：

* 用于创建一个完成端口对象
* 将一个句柄和完成端口关联到一起

在创建一个完成端口的时候，我们只需要填写NumberOfConcurrentThreads这个参数就可以了。它告诉操作系统一个完成端口上同时允许运行的线程最大数。在默认情况下，所开线程和CPU数量相同，但经验给我们一个公式

```
线程数 = CPU数 * 2 + 2
```

要使完成端口有用，你必须把它同一个或多个设备相关联。这也是调用CreateIoCompletionPort完成的。你要向该函数传递一个已有的完成端口的句柄，我们既然要处理网络事件，那也就是将客户的socket作为Handle传进去。和一个完成键（对你有意义的32位值，也就是一个指针，操作系统并不关心你传了什么）。每当你向端口关联一个设备时，系统向该完成端口的设备列表中加入一条信息记录

另一个API是

```
BOOL GetQueuedCompletionStatus(
	HANDLE CompletioPort,			//handle to completion port
	LPDWORD lpNumberOfBytes,		//bytes transferred
	PULONG_PTR lpCompletionKey,		//file Completion key
	LPOVERLAPPED *lpOverlapped,		//buffer
	DWORD dwMilliseconds			//optional timeout value
)
```

第一个参数指出了线程要监视哪一个完成端口。很多服务应用程序只是使用一个IO完成端口，所有的IO请求完成以后的通知都将发给该端口。简单地说，GetQueuedCompletionStatus使调用线程挂起，直到指定的端口的IO完成队列中出现一项或直到超时。同时IO完成端口相关联的第3个数据结构是使线程得到完成IO项的信息：传输的字节数，完成建和OVERLAPPED结构的地址。该信息是通过GetQueuedCompletionStatus的lpdwNumberOfBytesTransferred，lpdwCompletionKey和lpOverlapped参数返回给线程的

## echo例子

根据到目前为止已经讲到的东西，首先来构建一个frame。下面为你说明如何使用完成端口来开发一个echo服务器

大致如下

* 初始化WinSock
* 创建一个完成端口
* 根据服务器线程数创建一定数量的线程
* 准备好一个socket进行bind，然后listen
* 进入循环accept等待客户请求
* 创建一个数据结构容纳socket和其他相关信息
* 将连进来的socket同完成端口相关联
* 投递一个准备接收的请求

以后就是不断的重复5~8的过程

