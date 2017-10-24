>[《Windows下完成端口移植Linux下的epoll》](http://jazka.blog.51cto.com/809003/251759/)

先来说说Windows下的完成端口。完成端口号称是Windows下最复杂的异步IO操作。但是如果你想开发出具有高性能的、支持大量连接的网络服务程序的话，就必须将它拿下。这里假设你已经对完成端口有一定的了解了

下面引用一下幽默讲解Windows支持的五种Socket IO模型的例子来通俗的说一下完成端口究竟是怎么回事

老陈有一个在外地工作的女儿，不能经常回来，老陈和她通信联系。他们的信会被邮递员投递到他们的微软信箱里

我们平时使用的select模型，老陈每隔几分钟便到楼下看看是否有信。这样的方式会浪费老陈很多时间。同理，程序会阻塞在这里等待数据的到来，使得该进程（线程）无法进行其他操作，导致性能的降低

WSAAsyncSelect模型、WSAEventSelect模型同为事件触发模型。此时，只要有信到，微软就会主动通知老陈。此时，老陈只需要等待通知即可，在等待过程中可以做其他事情（Delphi的ScktComp就是使用该模型）

而Overlapped IO事件通知模型基本和上面两种（WSAAsyncSelect、WSAEventSelect）类似。只是，老陈不需要下楼取信了，他只需要告诉微软自己在几楼几号，微软就会把信送到老陈家

后来微软推出了Overlapped IO完成例程模型，老陈将自己拆信--阅读--回复的过程告诉微软，微软就会按照上述步骤去处理信件

但是，由于微软要处理的信件实在太多了，信箱经常崩溃。于是采用了新技术Completion Port来处理这些信件

通过Win32的重叠IO机制，应用程序可以提请一项IO操作，重叠的操作请求在后台完成，而同一时间提请操作的线程去做其他的事情。等重叠操作完成后线程收到有关的通知。而一个完成端口其实就是一个通知队列，由操作系统把已经完成的重叠IO请求的通知放入其中。当某项IO操作一旦完成，某个可以对该操作结果进行处理的工作者线程就会收到一则通知

完成端口的使用主要分为两步：

首先，创建完成端口

```
HANDLE hIocp;
hIocp = CreateIoCompletionPort(
    INVALID_HANDLE_VALUE,
    NULL,
    (ULONG_PTR)0,
    0);
if (NULL = hIocp){
    //Error
}
```

完成端口创建后，要把将使用该完成端口的套接字与之关联起来。方法是再次调用CreateIoCompletionPort()函数，第一个参数FileHandle设为套接字的句柄，第二个参数ExistingCompletionPort设为刚才创建的那个端口的句柄。以下代码创建一个套接字，并把它和前面创建的完成端口关联起来

```
SOCKET s;
s = socket(AF_INET, SOCK_STREAM, 0);

if(INVALID_SOCKET == s){
    //Error
}

if(CreateIoCompletionPort((HANDLE)s,
                          hIocp,
                          (ULONG_PTR)0,
                          0) == NULL)
{
    //Error
}
```

这时就完成了套接字和完成端口的关联操作。在这个套接字上进行的任何重叠操作都将通过完成端口发出完成通知

其次，使用API函数GetQueuedCompletionStatus来不断的监听查询某个完成端口的IO操作的结果

>通常来说，在主线程中都只创建一个完成端口，将所有的套接字都与此完成端口关联。而进行监听的查询线程数一般取CPU数量的两倍

```
BOOL GetQueuedCompletionStatus(
    HANDLE CompletionPort,        //handle to completion port
    LPDWORD lpNumberOfBytes,      //bytes transferred
    PULONG_PTR lpCompletionKey,   //file completion key
    LPOVERLAPPED *lpOverlapped,   //buffer
    DWORD dwMilliseconds          //optional timeout value
);
```

第一个参数指出了线程要监视哪一个完成端口。GetQueuedCompletionStatus使调用线程挂起，直到指定的端口的IO完成队列中出现了一项或直到超时。同时IO完成端口相关联的第三个数据结构是使线程得到完成IO项中的信息：传输的字节数，完成键和OVERLAPPED结构的地址。该信息是通过传递给GetQueuedCompletionStatus的lpdwNumberOfBytesTransferred，lpdwCompletionKey和lpOverlapped参数返回给线程的

注意lpOverlapped，这是很重要的一个数据结构，从这里你将获得你想要的数据，并进行判断处理。这里你可能会问，这个lpOverlapped数据结构是哪里来的，是什么类型的？接来下你就明白了

上面讨论了完成端口的使用，这其实是后期的处理，要想真正了解整个过程，还需要学习下面关于之前如何将发送和接收数据的IO操作提交

一个是API函数WSARecv从一个套接字接口接收数据

```
int WSAAPI WSARecv(SOCKET s, LPWSABUF lpBuffers,
    DWORD dwBufferCount, LPDWORD lpNumberOfBytesRecvd,
    LPINT lpFlags, LPWSAOVERLAPPED lpOverlapped,
    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
```

lpOverlapped：一个指向WSAOVERLAPPED结构的指针，在这个参数中就可以设置你要接收的数据结构

另一个API函数WASSend在一个已连接的套接字上发送数据

```
int WSAAPI WSASend(
    SOCKET s,
    LPWSABUF lpBuffers,
    DWORD dwBufferCount,
    LPDWORD lpNumberOfBytesSent,
    int iFlags,
    LPWSAOVERLAPPED lpOverlapped,
    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
```

同理，lpOverlapped用来设置发送数据结构

对完成端口来说，将一个套接字绑定到完成端口后，WSARecv和WSASend会立即返回，提高了系统的效率。可以调用GetQueuedCompletionStatus来判断WSARecv和WSASend是否完成。主线程接受到一个连接后，调用WSARecv等到该连接发送的数据（不阻塞，由完成端口实现数据的接收完毕判断）。在线程函数中接受完毕，然后用WSASend函数发送给客户数据（同样是不阻塞，直接返回，由完成端口判断数据是否发送完毕）。这样在线程函数中需要程序员自己设置状态来区分是发送完毕还是接收完毕

注意WSARecv只是向系统提交一个异步接收请求，这个请求会在有数据到达之后返回，并且放入完成队列通知工作线程，这个异步接收请求到此完成，继续提交请求是为了接收下一个数据包，也就是说，每次请求返回之后必须再次提交。WSASend也只是向系统提交一个异步发送请求，当发送成功后，需要提交WSARecv接收请求，因为发送是主动的，发送完毕后必然要等待接收对方的回复。如果不提交WSARecv接收请求，则对方发过来的数据后，完成端口不监听

>到目前为止，略懂，还得继续查资料！
