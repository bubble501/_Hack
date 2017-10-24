ScktComp是Delphi下对WinSock API封装之后的一个网络编程组件

关于Delphi下的网络网络编程我之前也总结过一系列文章：

* [《非阻塞IO网络编程存在的坑》](http://www.xumenger.com/socketapi-error-usage-20170404/)
* [《ScktComp的回调逻辑》](http://www.xumenger.com/scktcomp-test-20170329/)
* [《Delphi网络编程：ScktComp源码解析初步》](http://www.xumenger.com/02-Delphi-socket-source-20170103/)
* [《Delphi网络编程：ServerSocket/ClientSocket的线程分布》](http://www.xumenger.com/01-Delphi-socket-thread-20170103/)
* [《Delphi网络编程：发送和接收二进制数据》](http://www.xumenger.com/Delphi-binary-socket-20161222/)
* [《Delphi网络编程：FIX网络协议》](http://www.xumenger.com/Delphi-network-fix-20161221/)
* [《Delphi网络编程：阻塞和非阻塞》](http://www.xumenger.com/windows-Delphi-socket-20161011/)
* [《Delphi网络编程：使用ServerSocket/ClientSocket》](http://www.xumenger.com/windows-Delphi-socket-20161010/)
* [《Delphi网络编程：使用IdTcpServer/IdTcpClient》](http://www.xumenger.com/windows-Delphi-socket-20160929/)

另外关于网络协议、Windows/Linux平台下的网络编程也试着去看了很多书，查阅了大量的资料，整理了不少的文章，做了不少的小demo练习，包括我在目前的工作中也有很多直接使用网络编程进行开发的地方。但是我始终的感觉就是杂而不精，什么都知道一点，但什么都说不清

* TCP协议的细节是什么样的？
* 什么样的编程方式、编程逻辑适用于的网络编程？
* 各种不同的网络IO模型的运行逻辑是什么样的？
* TCP协议细节在Socket API层面对应有什么样的体现？

还是得去看相关的源码、进行详细的注解才能有助于自己对于网络原理、网络编程有深刻的理解。从现在开始我会去找一些著名的网络编程库、网络编程组件、网络编程业务实践的源码去阅读、注解；尽可能的阅读TCP协议细节的书、网络编程的书；同时自己也去尝试做一些或大或小的编程实践，追求能尽可能的对于网络原理、网络编程有深刻的理解

之前的tinyhttpd、webbench算是两个很小的TCP编程例子，逻辑简单。这里尝试去阅读ScktComp的源码以对Windows平台的网络编程先进行深刻的学习

>以往我学习网络编程，主要就是学习SockAPI的基本用法，编写一些极其简单的代码，比如服务端就是极其简单的：创建Socket、绑定端口、监听、接受连接、读写，但是这种模式只能处理一个客户端连接，而且明显很没有效率，针对高并发、大批量的客户端连接，完全没有太多的网络编程的经验，而Delphi自己封装的ScktComp就是一个很好的案例，学习其如何进行代码封装、如何利用线程，都是很好的！

>上面整理的这些文章有些有参考的意义，也有一些可能是错误的理解，在这个open里面会详细对ScktComp的源码进行注释！以充分学习网络编程

# ScktComp设计模式

**ScktComp的核心类：**

* TCustomWinSocket系列类
  * TClientWinSocket：继承自TCustomWinSocket
  * TServerWinSocket：继承自TCustomWinSocket
    * 因为客户端和服务端在TCP通信中有不同的逻辑，所以TCustomWinSocket封装一些相同的逻辑
    * 然后客户端、服务端对应的类分别实现自己独特的部分
    * 比如客户端、服务端都有Send、Read这样的发送、接收方法
    * 比如客户端有Connect发起连接的方法，而服务端没有
    * 比如服务端有Listen监听、Accept接受连接的方法，而客户端没有
  * TServerClientWinSocket，这个类中有一个TServerWinSocket类型的成员
    * 客户端Socket在客户端有一个Socket，其用TClientWinSocket进行封装表示
    * 同时TCP编程中，每个客户端连接到服务端后，会在服务端也生成一个Socket
    * TServerClientWinSocket就是用于在服务端表示一个客户端Socket对象！
* TServerAcceptThread
  * 该线程是TServerWinSocket类的一个成员
    * TServerWinSocket类中也有一个TServerAcceptThread的成员
    * 所以这种相互拥有的编码方式在设计模式中也很常用，因为对于双方都可以很方便的获取对方！
  * 该线程持续循环以持续接收客户端的连接
  * 专门的线程来监听端口，接受连接，可以保证及时响应每一个客户端连接
* TServerClientThread
  * 在服务端设置为阻塞模式的情况下，服务端会针对每个客户端连接创建一个该线程
  * 对应放在一个TList中进行管理
  * 服务端阻塞的情况下，每个客户端TServerClientWinSocket对应一个该线程
  * TServerClientThread有一个TServerWinSocket对象，表示其属于哪个服务端
    * 另外还有一个TServerClientWinSocket对象，表示其对应处理的客户端Socket
    * 但是在TServerClientWinSocket并没有一个TServerClientThread对象
* TAbstractSocket
  * 继承自TComponent，因为ScktComp作为Delphi的组件使用，所以需要继承TComponent
  * TCustomSocket，继承自TAbstractSocket
  * TClientSocket，继承自TCustomSocket
    * 其中有一个TClientWinSocket对象，主要就是通过这个对象实现网络通信的
    * 另外定义OnLookup、OnConnecting、OnConnect、OnDisConnect、OnRead、OnWrite、OnError属性
    * 这些属性，需要开发者在使用TClientSocket编程时，赋对应的函数，然后就可以在对应的时刻回调对应的函数
  * TCustomServerSocket，继承自TCustomSocket
  * TServerSocket，继承自TCustomServerSocket
    * 服务端编程主要是使用这个组件来进行编程
    * 定义了OnListen、OnAccept、OnGetThread、OnGetSocket、OnThreadStart、OnThreadEnd、OnClientConnect、OnClientDisConnect、OnClientRead、OnClientError属性
    * 这些属性，需要开发者在使用TServerSocket编程时，赋对应的函数，然后就可以在对应的时刻回调对应的函数
* TWinSocketStream
  * 继承自TStream，实现网络流读写

补充一下。在网络编程中Socket有三个层面的含义：

* 客户端Socket
* 服务端监听的Socket
* 服务端收到客户端连接后为每个客户端创建的Socket

**ScktComp核心线程：**

* TServerAcceptThread
  * 只有在服务端为阻塞模式下，该线程才会被启动
  * 服务端专门启动一个该线程，用于循环接受客户端的连接
  * 并为客户端的连接做初步的处理，将客户端放到一个List中进行管理
  * 专门启动一个线程来接收客户端连接，而不用关心客户端的读写
  * 如此是很好的拆分思想，让一个/一种线程只做一件事！
* TServerClientThread
  * 在服务端为阻塞的模式下，该线程有用
  * 服务端为阻塞的模式下，为每个客户端连接创建一个该线程
  * 当客户端断开连接的时候，对应的该线程并不销毁，而是暂时不工作，等待给它分配新的连接

**ScktComp网络IO模式**

ScktComp在客户端和服务端都分别支持阻塞和非阻塞的模式

通过研究ScktComp的设计模式、线程设计、IO模式，对于自己后续开发中编码设计、面向对象设计、设计模式、多线程、网络IO的应用有很大的帮助和参考

除了应用到设计模式，其中还应用了大量的回调逻辑，这部分也是很值得研究的！

其中封装Socket的方式，值得好好参考，后续在Linux平台实现一个网络编程库

**补充：如何有效阅读代码**

* 一般的套路、方法
  * 对于面向过程的语言，最好事先将函数调用树画出来
  * 对于面向对象的语言，最好事先将类图绘制出来
  * 对着接口文档、设计文档、数据库接口来阅读
  * 最好应用该源码编写一个简单的可运行的程序，然后断点跟踪其执行逻辑！
* 接下来谈一下针对ScktComp的源码怎么阅读
  * 因为自己有使用ScktComp开发的经验，所以从其暴露给开发者的接口入手研究
  * 从开发者使用的接口入手，一层一层往里看函数调用关系、逻辑
  * 边阅读源码，边添加注释
  * 边阅读源码，边试着去绘制流程图、类图等

# 最后再转载《Delphi下的SOCK编程》

[《Delphi下的SOCK编程》](http://www.cnblogs.com/linyawen/archive/2010/12/16/1908564.html)

本文是写给公司新来的程序员的，算是一点培训的教材。本文不会涉及太多的编程细节，只是简单地讲解在Delphi下进行WinSock编程最好了解的知识

>题外话：我认为学习编程就如同学习外语一样，最好的方式是你先学会如何去运用它，然后才是了解它的语言特性、语法之类的东西。不过很可惜，我们以前的外语交易使用了相反的过程。软件编程也是一样，在很多人的大学阶段，你更多的是学习那些理论知识，学习“语法”，这里，我丝毫没有贬低理论知识重要性的意思。理论知识和实践是相辅相成的，但一个恰当的学习方式，很多时候可以让学习者得到事半功倍的效果。例如你学习《算法与数据结构》中排序的概念，我们假设对此概念你学习的很出色，你脑子中有大量关于不同排序方法优劣的思想，你也自己琢磨出了很多奇妙的构思，但你没有丝毫编写代码的基本功，你所有的一些的nice idea都只能停留在你的脑子中，你甚至不知道在实践运用中，这些idea是否可行。实践是检验真理的唯一标准。倘若你先具备了一些可用于实践的基本技能，再去学习这些概念，我想，你可以很好的将“冒泡排序”实现出来，然后在这种实践和理论的结合下不断的前行

>本文会从Delphi中的TServerSocket等组件出发，边用边学，知道API以及那些所谓的“思想”

>“当你相信并不理解的东西时，你就会受罪了。迷信不是办法。” —— Stevice Wonder

RAD使得程序的编写更方便和便捷，即使没有经受专业的训练，依然可以写出完成一些功能的程序，在我现在的公司内部，有时候我会问公司的新员工，“你为什么这样写，你的代码是怎么运行的？”，“不知道，反正可以运行。”。我认为对于准备在Win32开发领域下做下去的程序员来说，这是悲哀的。前面已经说过了，本文的最终目的不是简单的组件使用，而是让读者可以了解WinSock的API以及Delphi中TServerSocket等VCL组件的编写。我始终认为对于一名合格的Win32程序员来说，无论你使用什么工具编写代码，你都应该深入底层，起码一应该了解Win API，否则的话，在面对那些组件错误的时候，你只能莫名其妙的扣着脑袋，在编程大道路上，最多也只能成为一名熟练的“搬运工”

阅读本文，你起码应该具备对Object Pascal的了解

## WinSock的基本概念

在Wi32平台下进行网络编程，对于众多的基层网络协议，WinSock是访问它们的首选接口。而且在每个Win32平台上，WinSock都以不同的形式存在着

要说的是，WinSock是网络编程接口，而不是协议。它从Unix的Berkeley（BSD）套接字方案借鉴了许多东西，后者能访问多种网络协议。在Win32环境中，WinSock接口最终成为一个真正的“与协议无关”接口，尤其是在WinSock 2发布之后

WinSock的API是建立在套接字基础上的。所谓套接字，就是一个指向传输提供者的句柄。Win32中，套接字不同于文件描述符，所以它是一个独立的类型：SOCKET

## 去看看源码

前面说过，本文的目的不是告诉你如何使用Delphi中这些用于WinSock编程的组件，我们的目的是WinSock API

那么现在就去看看TClientSocket和TServerSocket的源码，他们在ScktComp.pas中被实现

TClientSocket以及TServerSocket是一个组件，他们的目的是让我们可以建立典型的客户机/服务器模式的通讯程序

从设计的概念上说，他们并不负责通讯的具体处理，仔细观察他们的代码，你会发现二者有一个共同的地方，都有一个T\*\*\*WinSocket的私有变量，是的，T\*\*\*WinSocket才是真正完成对WinSock API封装的地方

## WinSock API简介

勿在浅沙筑高楼。在谈论TServerSocket等组件编写之前，这里先对WinSock中一些基本的概念和API函数做一个简单的说明

要用过WinSock建立通信，必须了解如何利用指定的协议为工作站定址。WinSock 2引入了几个新的、与协议无关的函数，它们可和任何一个地址家族一起使用；但大多数情况下，各协议家族都有自己的地址解析机制，要么通过一个函数，要么作为一个投给getsockopt的选项

因为目前网络编程中用的最多最普遍的也许就是TCP/IP协议了，所以这里主要介绍此写一下的WinSock编程

**1.IP**

网络协议（Internet Protocol，IP）是一种用于互联网的网络协议，已经广为人知。它可广泛用于大多数计算机操作系统上，也可用于大多数局域网LAN（比如办公室小型网络）和广域网WAN（比如说互联网）。从它的设计看来，IP是一个无连接的协议，不能保证数据传递万无一失。两个比它高级的协议（TCP和UDP）用于依赖IP协议的数据通信

**2.TCP**

面向连接的通信是通过“传输控制协议”（Transmission Control Protocol，TCP）来完成的，TCP提供两台计算机之间的可靠无错的数据传输。应用程序利用TCP进行通信时，源和目标之间会建立一个虚拟连接。这个连接一旦建立，两台计算机之间就可以把数据当做一个双向字节流进行交换

**3.UDP**

无连接通信是通过“用户数据报协议”（User Datagram Protocol，UDP）来完成的。UDP不保证可靠数据的传输，但能够向若干个目标发送数据，接收法子若干个源的数据。简单地说，如果一个客户机向服务器发送护具，这一数据会立即发出，不管服务器是否已经准备接收数据。如果服务器收到了客户机的数据，它不会确认收到与否。数据传输方法采用的是数据报

TCP和UDP两者都利用IP来进行数据传输，一般称为TCP/IP和UDP/IP。WinSock通过AF\_Inet地址家族为IP通信定址

**4.定址**

IP中，计算机都分配有一个IP地址，用一个32位数来表示，正式的称呼是“IPv4地址”。客户机需要通过TCP或UDP和服务器通信时，必须指定服务器的IP地址和端口号。另外，服务器打算监听接入客户机请求时，也必须指定一个IP和端口号。WinSock中，应用通过SOCKADDR\_IN结构来指定IP地址和服务端口信息，其在Delphi中的声明如下

```
sockaddr_in = record
	case Integer of
		0:(sin_family: u_short;
			sin_port:u_short;
			sin_addr: TInAddr;
			sin_zero: array[0..7] of Char);
		1:(sa_family:u_short;
			sa_data:array[0..13] of Char)
end;

TSockAddrIn = sockaddr_in;
```

在Delphi中，sockaddr\_in结构被声明为一个变体记录。sin\_family：字段必须设为AF\_INET，以告知WinSock我们此时正在使用IP地址家族

准备使用哪个TCP或UDP通信端口来标识服务器服务这一问题，则由sin\_port字段定义。在选择端口时，应用必须特别小心，因为有些可用端口是为“已知的”（即固定的）服务保留的（比如说文件传输协议FTP和超文本传输协议HTTP）。“已知的协议”，即固定协议，采用的端口由“互联网编号分配认证（IANA）”控制和分配，RFC 1700中说明编号。从本质上说，编口号分为下面三类：

* 0~1023由IANA控制，是为固定服务保留的
* 1024~49151是IANA列出来的、已注册的端口，供普通用户的普通用户进程或程序使用
* 49152~65535是动态和（或）私用端口

普通用户应该选择1024~49151之间的已注册端口，从而避免端口号已被另一个应用或系统服务所用。此外， 49152~65535之间的端口可自由使用，因为IANA这些端口上没有注册服务。在使用bind API函数时，如果一个应用和主机上的另一个应用采用的端口号绑定在一起，系统就会返回WinSock错误WSAEADDRUNUSE。sockaddr\_in结构的sin\_addr字段用于把一个IP地址保存为一个4字节的数，它是无符号长整数类型。根据这个字段的不同用法，还可表示一个本地货远程IP地址。IP地址一般是用“互联网标准点分表示法”（像a.b.c.d）指定的，每个字母代表一个字节数，从左到右分配一个4字节的无符号长整数。最后一个字段sin\_zero，只充当填充项的职责，以使sockaddr\_in结构和SOCKADDR结构的长度一样。一个有用的、名为inet\_addr的支持函数，可把一个点式IP地址转换成一个32位的无符号长整数。它的定义如下：

```
unsigned long inet_addr(
	const char FAR *cp
);
```

cp字段是一个空终止字符串，它认可点式表示法的IP地址。注意，这个函数把IP地址当做一个按网络字节顺序排列的32位无符号长整数返回

**1.特殊地址**

对于特定情况下的套接字行为，有两个特殊IP地址可对它们产生影响。特殊地址INADDR\_ANY允许服务器应用监听主机计算机上面每个网络接口上的客户机活动。一般情况下，在该地址绑定套接字和本地接口时，网络应用才利用这个地址来监听连接。如果你有一个多址系统，这个地址就允许一个独立应用接受自发多个接口的回应

特殊地址INADDR\_BROADCAST用于在一个IP网络中发送广播UDP数据包。要是用这个特殊地址，需要应用设置套接字选项SO\_BROADCAST

**2.字节顺序**

针对“大头”（big-endian）和“小头”（little-endian）形式的编号，不同的计算机处理器的表示方法有所不同，这由各自的设计决定。比如，Intel 86处理器上，用“小头”形式来表示多字节编号：字节的排序是从最无意义的字节到最有意义的字节。在计算机中把IP地址和端口号指定成多字节数时，这个数就按“主机字节”（host-byte）顺序来表示。但是，如果在网络上指定IP地址和端口号，“互联网联网标准”指定多字节值必须用“大头”形式来表示（从最有意义的字节到最无意义的字节），一般称之为“网络字节”（network-byte）顺序。有一系列的函数可用于多字节数的转换，把它们从主机字节顺序转换成网络字节顺序，反之亦然

下面四个API函数便将一个数从主机字节顺序转换成网络字节顺序：HTONL、htons、WSAHtons、WSAHtonl

下面这四个是前面四个函数的反向函数：它们把网络字节顺序转换成主机字节顺序：ntohl、WSANtohl、ntohs、WSANtohs

## WinSock的初始化

每个WinSock应用都必须加载WinSock DLL的相应版本。如果调用WinSock之前，没有加载WinSock库，这个函数就会返回一个SOCKET_ERROR，错误信息是WSANOTINITIALISED。加载Winsock库是通过调用WSAStartup函数实现的。这个函数在Delphi中的WinSock单元被定义如下：

```
function WSAStartup(wVersionRequired: word; var WSData: TWSAData): Integer; stdcall;
```

ScktComp中这样使用了此函数

```
procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise ESocketError.CreateResFmt(@sWindowsSocketError,
      [SysErrorMessage(ErrorCode), ErrorCode, 'WSAStartup']);
end;
```

### 错误检查和控制

对编写成功的WinSock应用程序而言，错误检查和控制是至关重要的。事实上，对WinSock函数来说，返回错误是非常常见的。但是，多数情况下，这些错误都是无关紧要的，通信仍可在套接字上进行。尽管其返回的值并非一成不变，但不成功的WinSock调用返回的最常见的值是SOCKET\_ERROR

在详细介绍各个API调用时，我们打算指出和各个错误对应的返回值。实际上，SOCKET\_ERROR常量是-1

如果调用一个WinSock函数，错误情况发生了，就可用WSAGETLastError函数来获得一段代码，这段代码明确地表明发生的情况。高函数的定义如下

```
function WSAGetLastError: Integer; stdcall;
```

发生错误之后，调用这个函数，就会返回所发生的特定错误的完整代码

## 服务器端的编程

因为TCP协议是一个面向连接的协议，它存在一个概念上的“服务器”端和“客户端”，在编码时，要区别对待

“服务器”在某种概念上我们可以理解为一个进程，它需要等待任意数量的客户机连接，以便为它们的请求提供服务。对服务器监听的连接来说，它必须在一个已知的名字上。在TCP/IP中，这个名字就是本地接口的IP地址，加上一个端口编号。每种协议都有一套不同的定址方案，所以有一种不同的命名方法，在WinSock中：

* 第一步是将指定协议的套接字绑定到它已知的名字上。这个过程是通过API调用bind来完成的
* 下一步是将套接字置为监听模式。这时，用listen函数来完成
* 最后，若一个客户试图建立连接，服务器必须通过accept或WSAAccept调用来接收连接

**1.socket**

```
function socket(af, Struct, protocol: Integer): TSocket; stdcall;
```

在加载Winsock DLL的相应版本之后，你要做的第一件事就是建立一个套接字了。在1.1版本中通过使用socket这个API来实现。第一个参数是你要使用的协议家族，第二个参数为套接字类型，最后一个参数指名你要使用的具体协议。下面的代码创建了一个使用IP协议家族中的TCP协议创建的流模式的套接字。

```
skc := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
```

**2.bind**

一旦为某个特定协议创建了套接字，就必须将套接字绑定到一个已知地址。bind函数可将指定的套接字同一个已知地址绑定到一起。该函数声明如下

```
function bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer; stdcall;
```

其中第一个参数s代表我们希望在上面等待客户连接的那个套接字第二个参数addr，针对自己打算使用的那个协议，必须把该参数填充一个地址缓冲区，第三个参数是要传递的、由协议决定的地址的长度。例如这样一段代码

```
var
  ErrorCode : integer;
  SockAdd_In : TSockAddrIn;
  ...
begin
  ...
  SockAdd_In.sin_family := PF_INET;
  SockAdd_In.sin_port := htons(FPort);
  SockAdd_In.sin_addr.S_addr := htonl(INADDR_ANY);
  ErrorCode := bind(FSock,SockAdd_In,sizeof(SockAdd_In));
```

一旦出错，bind就会返回SOCKET\_ERROR。对bind来说，最常见的错误是WSAEADDRINUSE。如使用的是TCP/IP，那么WSAEADDRINUSE就表示另一个进程已经同本地IP接口和端口号绑定到了一起，或者那个IP接口和端口号处于TIME\_WAIT状态。假如你针对一个套接字调用bind，但那个套接字已经绑定，便会返回WSAEFFAULT错误

**3.listen**

我们接下来要做的是将套接字置入监听模式。bind函数的作用只是将一个套接字和一个指定的地址关联在一起。指示一个套接字等候进入连接的API函数则是listen，其定义如下：

```
function listen(s: TSocket; backlog: Integer): Integer; stdcall;
```

第一个参数同样是限定套接字

backlog参数指定了正在等待连接的最大队列长度。这个参数非常重要，因为完全可能同时出现几个服务器连接请求。例如，假定backlog参数为2。如果三个客户机同时发出请求，那么头两个会被放在一个“待决”（等待处理）队列中，以便应用程序依次为它们提供服务

而第三个连接会造成一个WSAECONNREFUSED错误。注意，一旦服务器接受了一个连接，那个连接请求就会从队列中删去，以便别人可继续发出请求。backlog参数其实本身就存在着限制，这个限制是由基层的协议提供者决定的。如果出现非法值，那么会用与之最接近的一个合法值来取代。除此以外，对于如何知道实际的backlog值，其实并不存在一种标准手段。与listen对应的错误是非常直观的。到目前为止，最常见的错误是WSAEINVAL。该错误通常意味着，你忘记在listen之前调用bind。否则，与bind调用相反，使用listen时可能收到WSAEADDRINUSE。这个错误通常是在进行bind调用时发生的

**4.accept**

现在，我们已做好了接受客户连接的准备。这是通过accept或WSAAccept函数来完成的。accept格式如下：

```
function accept(s: TSocket; addr: PSockAddr; addrlen: PInteger): TSocket; stdcall;
```

其中，参数s是一个限定套接字，它处在监听模式。第二个参数应该是一个有效的SOCKADDR\_IN结构的地址，而addrlen应该是SOCKADDR\_IN结构的长度。对于属于另一种协议的套接字，应当用与那种协议对应的SOCKADDR结构来替换SOCKADDR\_IN。通过对accpet函数的调用，可为待决连接队列中的第一个连接请求提供服务。accept函数返回后，addr结构中会包含发出连接请求的那个客户机的IP地址信息，而addrlen参数则指出结构的长度。此外，accept会返回一个新的套接字描述符，它对应于已经接受的那个客户机连接。对于该客户机后续的所有操作，都应使用这个新套接字。至于原来那个监听套接字，它仍然用于接受其他客户机连接，而且仍处于监听模式

## 客户机API函数

客户机要简单得多，建立成功连接所需的步骤也要少得多。客户机只需三步操作：

* 用socket创建一个套接字
* 解析服务器名（以基层协议为准）
* 用connect初始化一个连接

**connect函数**

关于创建套接字和解析服务器名的方法，前面已有简单叙述，这里介绍最后一步连接的API函数。我们先来看看该函数的Winsock 1版本，其定义如下：
```
function connect(s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall;
```

该函数的参数是相当清楚的：s是即将在其上面建立连接的那个有效TCP套接字；name是针对TCP（说明连接的服务器）的套接字地址结构（SOCKADDR\_IN）；namelen则是名字参数的长度

## 数据传输

收发数据是网络编程中的主题。要在已经建立连接的套接字上接收数据，在Winsock 1版本中，可用这个API函数

```
int send (
    SOCKET s, 
    const char FAR * buf, 
    int len, 
    int flags 
   );
```

Delphi中声明如下：

```
function send(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall;
```

SOCKET参数是已建立连接的套接字，将在这个套接字上发送数据。第二个参数buf，则是字符缓冲区，区内包含即将发送的数据。第三个参数len，指定即将发送的缓冲区内的字符数。最后，flags可为0、MSG\_DONTROUTE或MSG\_OOB。另外，flags还可以是对那些标志进行按位“或运算”的一个结果

MSG\_DONTROUTE标志要求传送层不要将它发送的包路由出去，由基层的传送决定是否实现这个请求（例如，若传输协议不支持该选项，这一请求就会被忽略）

MSG\_OOB标志预示数据应该被带外发送

对返回数据而言，send返回发送的字节数；若发生错误就返回SOCKET\_ERROR。常见的错误是WSAECONNABORTED，这一错误一般发生在虚拟回路由于超时或协议有错而中断的时候。发生这种情况时，应该关闭这个套接字，因为它不能再用了。远程主机上的应用通过执行强行关闭或意外中断操作重新设置虚拟虚路时，或远程主机重新启动时，发生的则是WSAECONNRESET错误。再次提醒大家注意，发生这一错误时，应该关闭这个套接字。最后一个常见错误是WSAETIMEOUT，它发生在连接由于网络故障或远程连接系统异常死机而引起的连接中断时

同样地，在已建立了连接的套接字上接收数据也有个函数：

```
int recv (
    SOCKET s, 
    char FAR* buf, 
    int len, 
    int flags 
   );
```

Delphi中声明如下：

```
function recv(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall;
```

从API的原型中，我们可以看到，所有关系到收发数据的缓冲都属于简单的char类型。也就是说，这些函数没有“Unicode”版本。所有收发函数返回的错误代码都是SOCKET\_ERROR。一旦返回错误，系统就会调用WSAGetLastError获得详细的错误信息。最常见的错误是WSAECONNABORED和WSAECONNRESET。两者均涉及到即将关闭连接这一问题—要么通过超时，要么通过通信方关闭连接。另一个常见错误是WSAEWOULDBLOCK，一般出现在套接字处于非暂停模式或异步状态时。这个错误主要意味着指定函数暂不能完成

## 套接字I/O模型

共有五种类型的套接字I/O模型，可让WinSock应用程序对I/O进行管理，包括：

* select-选择
* WSAAsyncSelect-异步选择
* WSAEventSelect-事件选择
* overlapped-重叠
* completion port-完成端口

因为本文的出发点是Delphi的TServerSocket组件，基于此控件的实现，这里主要介绍select和WSAAsyncSelect两种I/O模型

**selec模型**

select模型是WinSock中最常见的I/O模型，之所以称其为select模型，是由于它的中心思想便是利用select函数，实现对I/O的管理

最初设计该模型时，主要面向的是某些使用Unix操作系统的计算机，他们采用的是Berkeley套接字方案。select模型已集成到WinSock 1.1中，它使那些想避免在套接字调用过程中被无辜“锁定”的应用程序，采用一种有序的方式，同时进行对多个套接字的管理

由于WinSock1.1向后兼容与Berkeley套接字实施方案，所以假如有一个Berkeley套接字应用使用了select函数，那么从理论角度讲，不需要对其进行任何修改，便可正常运行

利用select函数，我们判断套接字上是否存在数据，或者能否向一个套接字写入数据。之所以要设计这个函数，唯一的目的便是防止应用程序在套接字处于锁定模式中时，在一次I / O绑定调用（如send或recv）过程中，被迫进入“锁定”状态；同时防止在套接字处于非锁定模式中时，产生WSAEWOULDBLOCK错误。除非满足事先用参数规定的条件，否则select函数会在进行I/O操作时锁定

>锁定就是指阻塞

select函数原型如下

```
int select (
    int nfds, 
    fd_set FAR * readfds, 
    fd_set FAR * writefds, 
    fd_set FAR * exceptfds, 
    const struct timeval FAR * timeout 
   );
```

其中，每一个参数nfds会被忽略，之所以仍然要提供这个参数，只是为了保持与早期Berkeley套接字应用程序的兼容。大家可注意到三个fd\_set参数：一个用于检查可读性（readfds），一个用于检查可写性（writefds），另一个用于例外数据（exceptfds）。从根本上说，fd\_set数据类型代表着一系列特定套接字的集合

其中，readfds集合包括符合下述任何一个条件的套接字：

* 有数据可以读入
* 连接已经关闭、重设或中止
* 假如已调用了listen，而且一个连接正在建立，那么accept函数调用会成功

writefds集合包括符合下述任何一个条件的套接字：

* 有数据可以发出
* 如果已经完成了对一个非锁定连接调用的处理，连接就会成功

最后，exceptfds集合包括符合下述任何一个条件的套接字

* 假如已完成了对一个非锁定连接调用的处理，连接尝试就会失败
* 有带外（OOB）数据可供读取

例如，假定我们想测试一个套接字是否可读，必须将自己的套接字添加到readfds集合中，再等待select函数完成。select完成之后，必须判断自己的套接字是否仍为readfds集合的一部分。若答案是肯定的，便表明该套接字“可读”，可立即着手从它上面读取数据。在三个参数中（readfds、writefds、exceptfds），任何两个都可以是空值NULL，但至少有一个不能为空值！在任何不为空的集合中，必须包含至少一个套接字句柄；否则，select函数便没有任何东西可以等待。最后一个参数timeout对应的是一个指针，指向一个timeval结构体，用于决定select最多等待I/O操作完成多久的时间，如果timeout是一个NULL，那么select调用会无限期地“锁定”下去，直到至少有一个描述符符合指定的条件后结束

timeval的定义如下

```
timeval = record
	tv_sec: Longint;
	tv_usec: Longint;
end;
```

其中，tv\_sec字段以秒为单位指定等待时间；tv\_usec字段则以毫秒为单位指定等待时间。若将超时值设置为(0, 0)，表明select会立即返回，允许应用程序对select操作进行“轮询”。出于对性能方面的考虑，应避免这样的设置

select成功完成后，会在fd\_set结构中，返回刚好未完成的I/O操作的所有套接字句柄的总量。若超过timeval设定的时间，便会返回0。不管由于什么原因，假如select调用失败，都会返回SOCKET\_ERROR

用select对套接字进行监视之前，在自己的应用程序中，必须将套接字句柄分配给一个集合，设置好一个或全部读、写以及例外fd\_set结构

将一个套接字分配给任何一个集合后，再来调用select，便可知道一个套接字上是否发生了上述的I/O活动

```
var
	FdSet: TFDSet;
	TimeVal: TTimeVal;
	...
begin
	...

	while True do
	begin
		FD_ZERO(FdSet);
		FD_SET(FSock, FdSet);
		TimeVal.tv_sec := 0;
		TimeVal.tv_usec := 500;
		//使用select函数
		if (select(0, @FdSet, nil , nil, @TimeVal) > 0) then
		begin
			AddSize = Sizeof(Add);
			AcceptSock := accept(FSock, @Add, @AddSize);
			if AcceptSock <> INVALID_SOCKET then
			begin
				TSockReadThread.Create(AcceptSock, Memo1);
			end;
		end;
		Application.ProcessMessages;
	end;

	...
end;
```

**WSAAsyncSelect模型**

WinSock提供了一个有用的异步I/O模型。利用这个模型，应用程序可在一个套接字上，接收以Windows消息为基础的网络事件通知

具体的做法是在建立好一个套接字后，调用WSAAsyncSelec函数。该模型最早出现在WinSock的1.1版本中，用于帮助应用程序开发者面向一些早期的16位Windows平台，适应其“落后”的多任务消息环境。应用程序仍可从这种模型中得到好处，特别是它们用一个标准的Windows例程（常称为“winproc”），对窗口消息进行管理的时候

要想使用WSAAsyncSelect模型，程序必须具备一个窗口，然后有消息循环系统，我们通常会自定义一个消息，然后调用WSAAsyncSelect函数将此消息投递到指定的窗口句柄中。WSAAsyncSelect函数定义如下

```
int WSAAsyncSelect (
    SOCKET s, 
    HWND hWnd, 
    unsigned int wMsg, 
    long lEvent 
   );
```

其中，s参数指定的是我们感兴趣的那个套接字，hWnd参数指定的是一个窗口句柄，它对应于网络事件发生后，想要收到通知消息的那个窗口或对话框。wMsg参数指定在发生网络事件时，打算接收的消息。该消息会投递到由hWnd窗口句柄指定的那个窗口。最后一个参数是lEvent，它指定的是一个位掩码，对应于一系列网络事件的组合，应用程序感兴趣的便是这一系列事件。大多数应用程序通常感兴趣的网络事件类型包括：FD\_READ、FD\_WRITE、FD\_ACCEPT、FD\_CONNECT和FD\_CLOSE。当然，到底使用FD\_ACCEPT，还是使用FD\_CONNECT类型，要取决于应用程序的身份到底是客户机还是服务器。如应用程序 同时对多个网络事件有兴趣，只需对各种类型执行一次简单的按位OR运算，然后将它们分配给lEvent就可以了

特别要注意的是，多个事件务必在一个套接字上一次注册！另外还要注意的是，一旦在某个套接字上允许了事件通知，那么以后除非明确调用closesocket命令，或者由应用程序针对那个套接字调用了WSAAsyncSelect，从而更改了注册的网络事件类型，否则的话，事件通知会永远有效！若将lEvent参数设为0，效果相当于停止在套接字上进行的所有网络事件通知

若应用程序针对一个套接字调用了WSAAsyncSelect，那么套接字的模式会从“锁定”自动变为“非锁定”

* FD\_READ 应用程序想要接收有关是否可读的通知，以便读入数据
* FD\_WRITE 应用程序想要接收有关是否可写的通知，以便写入数据
* FD\_OOB 应用程序想接收是否有带外（OOB）数据抵达的通知
* FD\_ACCEPT 应用程序想接收与进入连接有关的通知
* FD\_CONNECT 应用程序想接收与一次连接或者多点join操作完成的通知
* FD\_CLOSE 应用程序想接收与套接字关闭有关的通知
* FD\_QOS 应用程序想接收套接字“服务质量”（QoS）发生更改的通知
* FD\_GROUP\_QOS 应用程序想接收套接字组“服务质量”发生更改的通知（现在没什么用处，为未来套接字组的使用保留）
* FD\_ROUTING\_INTERFACE_CHANGE 应用程序想接收在指定的方向上，与路由接口发生变化的通知
* FD\_ADDRESS\_LIST\_CHANGE应用程序想接收针对套接字的协议家族，本地地址列表发生变化的通知

应用程序在一个套接字上成功调用了WSAAsyncSelect之后，应用程序会在hWnd窗口句柄参数对应的窗口例程中，以Windows消息的形式，接收网络事件通知

就我们的情况而言，感兴趣的是WSAAsyncSelect调用中定义的消息。wParam参数指定其上面发生了一个网络事件的套接字。假若同时为了这个窗口例程分配了多个套接字，这个参数的重要性便显示出来了。在lParam参数中，包含了两方面重要的信息。其中， lParam的低字（低位字）指定了已经发生的网络事件，而lParam的高字（高位字）包含了可能出现的任何错误代码

网络事件消息抵达一个窗口例程后，应用程序首先应检查lParam的高字位，以判断是否在套接字上发生了一个网络错误。若应用程序发现套接字上没有产生任何错误，接着便应调查到底是哪个网络事件类型，造成了这条Windows消息的触发—具体的做法便是读取lParam之低字位的内容。此时可使用另一个特殊的宏：WSAGetSelectEvent（在Delphi中，它以一个函数的形式存在），用它返回lParam的低字部分
