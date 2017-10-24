{ *********************************************************************** }
{                                                                         }
{ Delphi Runtime Library                                                  }
{                                                                         }
{ Copyright (c) 1997-2001 Borland Software Corporation                    }
{                                                                         }
{ *********************************************************************** }

{*******************************************************}
{       Windows socket components                       }
{*******************************************************}

unit ScktComp;

interface

uses SysUtils, Windows, Messages, Classes, WinSock, SyncObjs;

const
  //根据变量名翻译：Socket消息
  CM_SOCKETMESSAGE = WM_USER + $0001;
  CM_DEFERFREE = WM_USER + $0002;
  CM_LOOKUPCOMPLETE = WM_USER + $0003;

type
  //定义Socket通信中的异常类，继承自Exception
  ESocketError = class(Exception);

  //Socket消息结构体
  TCMSocketMessage = record
    Msg: Cardinal;
    Socket: TSocket;
    SelectEvent: Word;
    SelectError: Word;
    Result: Longint;
  end;

  //LookupComplete 到底是个什么东西？
  TCMLookupComplete = record
    Msg: Cardinal;
    LookupHandle: THandle;
    AsyncBufLen: Word;
    AsyncError: Word;
    Result: Longint;
  end;

  //ScktComp中定义的几个类
  TCustomWinSocket = class;             //服务端、客户端Socket的父类
  TCustomSocket = class;                //Delphi组件用到的类，保证ServerSocket、ClientSocket都继承自此类
  TServerAcceptThread = class;          //服务端Accept客户端连接的线程
  TServerClientThread = class;          //阻塞模式下，对于每个客户端连接，服务端专门创建一个该线程去进行处理
  TServerWinSocket = class;             //服务端监听Socket
  TServerClientWinSocket = class;       //服务端表示客户端连接的Socket

  //服务端IO 类型：非阻塞、阻塞
  TServerType = (stNonBlocking, stThreadBlocking);
  //客户端IO 类型：非阻塞、阻塞
  TClientType = (ctNonBlocking, ctBlocking);
  //异步方式：读异步、写异步、带外异常、接受连接异步、连接异步、关闭连接异步
  TAsyncStyle = (asRead, asWrite, asOOB, asAccept, asConnect, asClose);
  //异步方式集合：因为同时可以选择多种异步方式
  TAsyncStyles = set of TAsyncStyle;
  //Socket事件：Lookup？、正在连接、连接、断开连接、监听、接受连接、发送数据、接收数据
  TSocketEvent = (seLookup, seConnecting, seConnect, seDisconnect, seListen, seAccept, seWrite, seRead);
  //Lookup状态？什么是lookup
  TLookupState = (lsIdle, lsLookupAddress, lsLookupService);
  //错误事件：通用错误、发送错误、接收错误、连接错误、断开连接错误、接受连接错误、Lookup错误
  TErrorEvent = (eeGeneral, eeSend, eeReceive, eeConnect, eeDisconnect, eeAccept, eeLookup);

  //2017-03-30，问题：什么是Lookup

  //定义Socket事件函数指针类型
  TSocketEventEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent) of object;
  //定义Socket错误事件函数指针类型
  TSocketErrorEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer) of object;
  //？？
  TGetSocketEvent = procedure (Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket) of object;
  //？？
  TGetThreadEvent = procedure (Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread) of object;
  //？？
  TSocketNotifyEvent = procedure (Sender: TObject; Socket: TCustomWinSocket) of object;

  TCustomWinSocket = class
  private
    FSocket: TSocket;                   //TSocket = u_int;
    FConnected: Boolean;                //是否连接成功
    FSendStream: TStream;               //发送流
    FDropAfterSend: Boolean;
    FHandle: HWnd;
    FAddr: TSockAddrIn;                 //地址
    FAsyncStyles: TASyncStyles;         //TAsyncStyles = set of TAsyncStyle;
    FLookupState: TLookupState;         //TLookupState = (lsIdle, lsLookupAddress, lsLookupService);
    FLookupHandle: THandle;             //
    FOnSocketEvent: TSocketEventEvent;  //TSocketEventEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent) of object;
    FOnErrorEvent: TSocketErrorEvent;   //TSocketErrorEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer) of object;
    FSocketLock: TCriticalSection;      //使用临界区保证线程安全
    FGetHostData: Pointer;
    FData: Pointer;
    // Used during non-blocking host and service lookups
    FService: string;                   //
    FPort: Word;                        //端口
    FClient: Boolean;                   //表示该Socket是不是客户端
    FQueueSize: Integer;                //接受连接队列大小
    function SendStreamPiece: Boolean;  
    procedure WndProc(var Message: TMessage);                                               //在该函数内部分发消息
    procedure CMLookupComplete(var Message: TCMLookupComplete); message CM_LOOKUPCOMPLETE;  //该函数用于响应CM_LOOKUPCOMPLETE消息
    procedure CMSocketMessage(var Message: TCMSocketMessage); message CM_SOCKETMESSAGE;     //该函数用于响应CM_SOCKETMESSAGE消息
    procedure CMDeferFree(var Message); message CM_DEFERFREE;                               //该函数用于响应CM_DEFERFREE消息
    procedure DeferFree;                //？？
    procedure DoSetAsyncStyles;         //？？
    function GetHandle: HWnd;           //？？
    function GetLocalHost: string;      //获取本机的Host名称
    function GetLocalAddress: string;   //获取本机的IP地址
    function GetLocalPort: Integer;     //获取本机Socket的端口号
    function GetRemoteHost: string;     //获取对方机器的Host名称
    function GetRemoteAddress: string;  //获取对方机器的IP地址
    function GetRemotePort: Integer;    //获取对方机器Socket的端口号
    function GetRemoteAddr: TSockAddrIn;//获取对方机器的地址（IP、端口）信息
  protected
    //以异步的方式初始化Socket
    procedure AsyncInitSocket(const Name, Address, Service: string; Port: Word; QueueSize: Integer; Client: Boolean);
    //发起连接？
    procedure DoOpen;
    //监听？
    procedure DoListen(QueueSize: Integer);
    //初始化Socket
    function InitSocket(const Name, Address, Service: string; Port: Word; Client: Boolean): TSockAddrIn;
    //回调对应的SocketEvent事件处理方法
    procedure Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent); dynamic;
    //回调对应的ErrorEvent错误处理方法
    procedure Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); dynamic;
    //设置异步方式
    procedure SetAsyncStyles(Value: TASyncStyles);
  public
    constructor Create(ASocket: TSocket);
    destructor Destroy; override;
    procedure Close;
    procedure DefaultHandler(var Message); override;
    procedure Lock;      //加锁
    procedure Unlock;    //解锁
    //服务端监听端口
    procedure Listen(const Name, Address, Service: string; Port: Word; QueueSize: Integer; Block: Boolean = True);
    //打开Socket
    procedure Open(const Name, Address, Service: string; Port: Word; Block: Boolean = True);
    //服务端接收客户端连接
    procedure Accept(Socket: TSocket); virtual;
    //发起连接
    procedure Connect(Socket: TSocket); virtual;
    //断开连接
    procedure Disconnect(Socket: TSocket); virtual;
    //从TCP缓冲区中读数据
    procedure Read(Socket: TSocket); virtual;
    //往TCP缓冲区中写数据
    procedure Write(Socket: TSocket); virtual;
    //Lookup到底是啥？
    function LookupName(const name: string): TInAddr;
    function LookupService(const service: string): Integer;

    //获取当前TCP缓冲区中数据量，参考文章：http://www.xumenger.com/scktcomp-test-20170329/
    function ReceiveLength: Integer;
    //从TCP缓冲区中读取数据，参考文章：http://www.xumenger.com/delphi-binary-socket-20161222/
    function ReceiveBuf(var Buf; Count: Integer): Integer;
    //以文本形式从TCP缓冲区中读取数据，如果传输的是二进制数据，这种方法就不可用
    function ReceiveText: string;
    //发送数据
    function SendBuf(var Buf; Count: Integer): Integer;
    //发送流，比如参数是文件流，就可以实现将文件中的内容发送出去
    function SendStream(AStream: TStream): Boolean;
    //？？
    function SendStreamThenDrop(AStream: TStream): Boolean;
    //发送文本
    function SendText(const S: string): Integer;

    property LocalHost: string read GetLocalHost;        //获取本机Host名
    property LocalAddress: string read GetLocalAddress;  //获取本机IP地址
    property LocalPort: Integer read GetLocalPort;       //获取本机端口

    property RemoteHost: string read GetRemoteHost;      //获取对方Host名
    property RemoteAddress: string read GetRemoteAddress;//获取对方IP
    property RemotePort: Integer read GetRemotePort;     //获取对方端口
    property RemoteAddr: TSockAddrIn read GetRemoteAddr; //获取对方地址信息

    property Connected: Boolean read FConnected;         //是否连接成功
    property Addr: TSockAddrIn read FAddr;
    property ASyncStyles: TAsyncStyles read FAsyncStyles write SetAsyncStyles;  //异步方式
    property Handle: HWnd read GetHandle;
    property SocketHandle: TSocket read FSocket;
    property LookupState: TLookupState read FLookupState;//Lookup到底是啥？？

    //Socket事件回调
    property OnSocketEvent: TSocketEventEvent read FOnSocketEvent write FOnSocketEvent;
    //Socket错误回调
    property OnErrorEvent: TSocketErrorEvent read FOnErrorEvent write FOnErrorEvent;

    property Data: Pointer read FData write FData;
  end;

  //TClientWinSocket继承自TCustomWinSocket，实现客户端独有的一些功能
  //TCustomWinSocket里面封装了客户端和服务端相同的逻辑
  //然后，TClientWinSocket实现客户端特有的逻辑
  //这就是在编程中重要的抽象，将相同的部分抽象出来，不同的部分通过继承的方式由对应的子类实现
  TClientWinSocket = class(TCustomWinSocket)
  private
    FClientType: TClientType;                    //客户端类型，TClientType = (ctNonBlocking, ctBlocking);
  protected
    procedure SetClientType(Value: TClientType); //设置客户端类型
  public
    procedure Connect(Socket: TSocket); override;//只有客户端有发起连接的逻辑，所以客户端需要实现连接方法
    property ClientType: TClientType read FClientType write SetClientType;
  end;

  //TServerClientWinSocket继承自TCustomWinSocket
  //该类用于在服务端表示一个客户端连接对象
  TServerClientWinSocket = class(TCustomWinSocket)
  private
    FServerWinSocket: TServerWinSocket;
  public
    constructor Create(Socket: TSocket; ServerWinSocket: TServerWinSocket);
    destructor Destroy; override;

    property ServerWinSocket: TServerWinSocket read FServerWinSocket;
  end;

  TThreadNotifyEvent = procedure (Sender: TObject; Thread: TServerClientThread) of object;

  //TServerWinSocket继承自TCustomWinSocket，实现服务端一些特有的功能
  //比如服务端有Listen、Accept功能，而客户端并没有
  TServerWinSocket = class(TCustomWinSocket)
  private
    FServerType: TServerType;                //服务端类型：TServerType = (stNonBlocking, stThreadBlocking);
    FThreadCacheSize: Integer;               //线程管理List中线程数？
    FConnections: TList;                     //用于存放已经连接的客户端Socket，存放的类型是：（AClient: TServerClientWinSocket）
    FActiveThreads: TList;                   //存放的线程是AThread: TServerClientThread ，服务端设置为阻塞的情况下，每个客户端连接对应创建一个TServerClientThread线程去处理该客户端的请求
    FListLock: TCriticalSection;             //加锁保护，保证线程安全
    FServerAcceptThread: TServerAcceptThread;//该线程用于循环接收客户端的连接
    FOnGetSocket: TGetSocketEvent;           //TGetSocketEvent = procedure (Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket) of object;
    FOnGetThread: TGetThreadEvent;           //TGetThreadEvent = procedure (Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread) of object;

    //TThreadNotifyEvent = procedure (Sender: TObject; Thread: TServerClientThread) of object;
    FOnThreadStart: TThreadNotifyEvent;      //该回调函数用于启动一个TServerClientThread线程，在ThreadStart方法中回调该函数指针
    FOnThreadEnd: TThreadNotifyEvent;        //该回调函数用于结束一个TServerClientThread线程，在ThreadEnd方法中回调该函数指针

    //TSocketNotifyEvent = procedure (Sender: TObject; Socket: TCustomWinSocket) of object;
    FOnClientConnect: TSocketNotifyEvent;    //收到客户端连接后的事件
    FOnClientDisconnect: TSocketNotifyEvent; //客户端断开连接的事件
    FOnClientRead: TSocketNotifyEvent;
    FOnClientWrite: TSocketNotifyEvent;      //

    //TSocketErrorEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer) of object;
    FOnClientError: TSocketErrorEvent;       //客户端连接发生错误的事件

    procedure AddClient(AClient: TServerClientWinSocket);         //往FConnections: TList 中添加一个连接成功的客户端Socket
    procedure RemoveClient(AClient: TServerClientWinSocket);      //从FConnections: TList 移除一个连接成功的客户端Socket
    procedure AddThread(AThread: TServerClientThread);            //往FActiveThreads 中添加一个 TServerClientThread线程
    procedure RemoveThread(AThread: TServerClientThread);         //从FActiveThreads 中移除一个 TServerClientThread线程
    //根据不同的SocketEvent类型，回调不同的事件回调函数指针，具体处理哪些SocketEvent，详细参见对应实现代码
    procedure ClientEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
    //根据不同的ErrorEvent类型，回调不同的错误回调函数指针
    procedure ClientError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    function GetActiveConnections: Integer;                            //获取依然活着的客户端连接个数，也就是返回 FConnections.Count
    function GetActiveThreads: Integer;                                //获取依然活着的线程个数，也就是返回 FActiveThreads.Count
    function GetConnections(Index: Integer): TCustomWinSocket;         //从FConnections中获取第Index个客户端连接Socket
    function GetIdleThreads: Integer;
  protected
    //为每个客户端TServerClientWinSocket，创建一个TServerClientThread线程
    function DoCreateThread(ClientSocket: TServerClientWinSocket): TServerClientThread; virtual;
    procedure Listen(var Name, Address, Service: string; Port: Word; QueueSize: Integer);   //监听服务端端口
    procedure SetServerType(Value: TServerType);                      //设置服务端的类型：阻塞、还是非阻塞
    procedure SetThreadCacheSize(Value: Integer);                     //设置线程池个数
    procedure ThreadEnd(AThread: TServerClientThread); dynamic;       //结束一个TServerClientThread线程
    procedure ThreadStart(AThread: TServerClientThread); dynamic;     //启动一个TServerClientThread
    function GetClientSocket(Socket: TSocket): TServerClientWinSocket; dynamic;     //根据原生的TSocket，创建一个TServerClientWinSocket，在服务端每次收到一个客户端连接时用得到
    function GetServerThread(ClientSocket: TServerClientWinSocket): TServerClientThread; dynamic;   //为TServerClientWinSocket创建一个专门的TServerClientThread线程来处理这个TServerClientWinSocket的相关请求
    procedure ClientRead(Socket: TCustomWinSocket); dynamic;          //接收客户端发送的数据，其中回调FOnClientRead(Self, Socket);
    procedure ClientWrite(Socket: TCustomWinSOcket); dynamic;         //给客户端发送数据，其中回调FOnClientConnect(Self, Socket);
    procedure ClientConnect(Socket: TCustomWinSOcket); dynamic;       //收到客户端连接，其中回调FOnClientConnect(Self, Socket);
    procedure ClientDisconnect(Socket: TCustomWinSOcket); dynamic;    //客户端断开连接，其中回调FOnClientDisconnect(Self, Socket);
    //客户端连接出错，其中回调FOnClientError(Self, Socket, ErrorEvent, ErrorCode);
    procedure ClientErrorEvent(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); dynamic;
  public
    constructor Create(ASocket: TSocket);
    destructor Destroy; override;
    procedure Accept(Socket: TSocket); override;        //接收客户端连接
    procedure Disconnect(Socket: TSocket); override;    //和客户端断开连接
    function GetClientThread(ClientSocket: TServerClientWinSocket): TServerClientThread;   //获取TServerClientWinSocket对应的处理线程
    property ActiveConnections: Integer read GetActiveConnections;                         //获取当前活着的客户端连接个数
    property ActiveThreads: Integer read GetActiveThreads;                                 //获取当前或者的客户端处理线程个数
    property Connections[Index: Integer]: TCustomWinSocket read GetConnections;            //获取第Index个客户端连接
    property IdleThreads: Integer read GetIdleThreads;                                     //
    property ServerType: TServerType read FServerType write SetServerType;                 //获取服务端类型：阻塞、非阻塞
    property ThreadCacheSize: Integer read FThreadCacheSize write SetThreadCacheSize;      //？
    property OnGetSocket: TGetSocketEvent read FOnGetSocket write FOnGetSocket;            //？
    property OnGetThread: TGetThreadEvent read FOnGetThread write FOnGetThread;            //？
    property OnThreadStart: TThreadNotifyEvent read FOnThreadStart write FOnThreadStart;   //启动线程回调
    property OnThreadEnd: TThreadNotifyEvent read FOnThreadEnd write FOnThreadEnd;         //结束线程回调
    property OnClientConnect: TSocketNotifyEvent read FOnClientConnect write FOnClientConnect;           //收到客户端连接回调
    property OnClientDisconnect: TSocketNotifyEvent read FOnClientDisconnect write FOnClientDisconnect;  //客户端断开连接回调
    property OnClientRead: TSocketNotifyEvent read FOnClientRead write FOnClientRead;                    //？
    property OnClientWrite: TSocketNotifyEvent read FOnClientWrite write FOnClientWrite;                 //？
    property OnClientError: TSocketErrorEvent read FOnClientError write FOnClientError;                  //？
  end;

  //该线程专门用于循环接收客户端连接
  TServerAcceptThread = class(TThread)
  private
    FServerSocket: TServerWinSocket;
  public
    constructor Create(CreateSuspended: Boolean; ASocket: TServerWinSocket);
    procedure Execute; override;

    property ServerSocket: TServerWinSocket read FServerSocket;
  end;

  //该线程在服务端为阻塞模式下启用，阻塞模式下，服务端会针对每个客户端连接单独创建一个该线程
  TServerClientThread = class(TThread)
  private
    FClientSocket: TServerClientWinSocket;     //对应的客户端连接Socket
    FServerSocket: TServerWinSocket;           //其所属的服务端Socket
    FException: Exception;                     //异常
    {TEvent是在SyncObjs单元中定义的类，其实THandleObject类的子类，要使用它需要先uses SyncObjs
    TEvent若在多线程环境中可用于与其他线程同步；若在单线程环境中可用于调整响应不同的异步事件（如系统消息或用户动作）的代码段
    TEvent同样可以使用WaitFor()和WaitForMultiple()函数
    但要注意的是，TEvent类并没有实现Acquire函数
    Delphi中定义了一个更简单的事件类，TSimpleEvent类，但从源码上看，该类仅有TSimpleEvent=class(TEvent);一句，并没定义任何属于TSimpleEvent的成员，估计是作为向后兼容而存在
      TEvent.SetEvent()   置为发信号状态
      TEvent.ReSetEvent() 置为不发信号状态
      TEvent.WaitFor()    等待，直到调用了SetEvent()将其置为发信号状态
      TEvent使用的是心信号量：FEvent: TSemaphore;}
    FEvent: TSimpleEvent;                      //
    FKeepInCache: Boolean;                     //？？
    FData: Pointer;                            //？？
    procedure HandleEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
    procedure HandleError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure DoHandleException;
    procedure DoRead;
    procedure DoWrite;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure ClientExecute; virtual;         //循环使用select模型去读、写客户端连接Socket
    procedure Event(SocketEvent: TSocketEvent); virtual;                        //回调对应TServerWinSocket的ClientEvent方法
    procedure Error(ErrorEvent: TErrorEvent; var ErrorCode: Integer); virtual;  //回调对应TServerWinSocket的ClientError方法
    procedure HandleException; virtual;
    //让这个线程重新活过来，当服务端为每个客户端创建一个线程的时候，如果该客户端断开连接了，那么这个线程就“死了”，但是不释放该线程
    //等到下一个客户端连接过来的时候，就从服务端的线程链表中取出一个“死了”的线程，在让其来处理新的客户端连接
    //如此就可以不用频繁的创建、销毁线程，节省了很多不必要的资源
    procedure ReActivate(ASocket: TServerClientWinSocket);
    function StartConnect: Boolean;                //？？
    function EndConnect: Boolean;                  //？？
  public
    constructor Create(CreateSuspended: Boolean; ASocket: TServerClientWinSocket);
    destructor Destroy; override;

    property ClientSocket: TServerClientWinSocket read FClientSocket;
    property ServerSocket: TServerWinSocket read FServerSocket;
    property KeepInCache: Boolean read FKeepInCache write FKeepInCache;
    property Data: Pointer read FData write FData;
  end;

  TAbstractSocket = class(TComponent)
  private
    FActive: Boolean;           //用于判断客户端/服务器是否启动
    FPort: Integer;             //端口号
    FAddress: string;           //IP 地址
    FHost: string;              //Host 名称
    FService: string;
    //回调：Event(Socket, SocketEvent);
    procedure DoEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
     //回调：Error(Socket, ErrorEvent, ErrorCode);
    procedure DoError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  protected
    //抽象方法，处理Socket事件，需要子类实现
    procedure Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent); virtual; abstract;
    //抽象方法，处理Socket错误，需要子类实现
    procedure Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); virtual; abstract;
    procedure DoActivate(Value: Boolean); virtual; abstract;
    procedure InitSocket(Socket: TCustomWinSocket);         //初始化Socket
    procedure Loaded; override;                             //？？
    procedure SetActive(Value: Boolean);                    //
    procedure SetAddress(Value: string);                    //设置IP地址
    procedure SetHost(Value: string);                       //设置Host名称
    procedure SetPort(Value: Integer);                      //设置端口
    procedure SetService(Value: string);                    //？？
    property Active: Boolean read FActive write SetActive;
    property Address: string read FAddress write SetAddress;
    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Service: string read FService write SetService;
  public
    procedure Open;                                         //Active := True;
    procedure Close;                                        //Active := False;
  end;

  //TCustomSocket继承自TAbstractSocket
  //之前说的连接、收数据等事件，还有错误的回调处理方法，就是开发者在这里通过指定FOnLookup、FOnConnect等设置的
  //等到对应的事件/错误出现的时候，在该类的Event、Error方法中根据事件/错误的类型回调开发者注册的对应方法
  TCustomSocket = class(TAbstractSocket)
  private
    FOnLookup: TSocketNotifyEvent;             //？？
    FOnConnect: TSocketNotifyEvent;            //连接事件
    FOnConnecting: TSocketNotifyEvent;         //正在连接事件
    FOnDisconnect: TSocketNotifyEvent;         //断开连接事件
    FOnListen: TSocketNotifyEvent;             //监听端口事件
    FOnAccept: TSocketNotifyEvent;             //接收客户端连接事件
    FOnRead: TSocketNotifyEvent;               //接收网络数据事件
    FOnWrite: TSocketNotifyEvent;              //发送网络数据事件
    FOnError: TSocketErrorEvent;               //发生错误
  protected
    procedure Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent); override;
    procedure Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); override;
    property OnLookup: TSocketNotifyEvent read FOnLookup write FOnLookup;
    property OnConnecting: TSocketNotifyEvent read FOnConnecting write FOnConnecting;
    property OnConnect: TSocketNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TSocketNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnListen: TSocketNotifyEvent read FOnListen write FOnListen;
    property OnAccept: TSocketNotifyEvent read FOnAccept write FOnAccept;
    property OnRead: TSocketNotifyEvent read FOnRead write FOnRead;
    property OnWrite: TSocketNotifyEvent read FOnWrite write FOnWrite;
    property OnError: TSocketErrorEvent read FOnError write FOnError;
  end;

  //网络流
  TWinSocketStream = class(TStream)
  private
    FSocket: TCustomWinSocket;
    FTimeout: Longint;
    FEvent: TSimpleEvent;
  public
    constructor Create(ASocket: TCustomWinSocket; TimeOut: Longint);
    destructor Destroy; override;
    function WaitForData(Timeout: Longint): Boolean;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property TimeOut: Longint read FTimeout write FTimeout;
  end;

  //客户端编程组件，在使用ScktComp进行客户端编程时，就是用这个组件
  TClientSocket = class(TCustomSocket)
  private
    FClientSocket: TClientWinSocket;
  protected
    procedure DoActivate(Value: Boolean); override;
    function GetClientType: TClientType;
    procedure SetClientType(Value: TClientType);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Socket: TClientWinSocket read FClientSocket;
  //通过publised关键字将这些属性在开发面板上显示
  published
    property Active;
    property Address;
    property ClientType: TClientType read GetClientType write SetClientType;
    property Host;
    property Port;
    property Service;
    property OnLookup;
    property OnConnecting;
    property OnConnect;
    property OnDisconnect;
    property OnRead;
    property OnWrite;
    property OnError;
  end;

  TCustomServerSocket = class(TCustomSocket)
  protected
    FServerSocket: TServerWinSocket;
    procedure DoActivate(Value: Boolean); override;
    function GetServerType: TServerType;
    function GetGetThreadEvent: TGetThreadEvent;
    function GetGetSocketEvent: TGetSocketEvent;
    function GetThreadCacheSize: Integer;
    function GetOnThreadStart: TThreadNotifyEvent;
    function GetOnThreadEnd: TThreadNotifyEvent;
    function GetOnClientEvent(Index: Integer): TSocketNotifyEvent;
    function GetOnClientError: TSocketErrorEvent;
    procedure SetServerType(Value: TServerType);
    procedure SetGetThreadEvent(Value: TGetThreadEvent);
    procedure SetGetSocketEvent(Value: TGetSocketEvent);
    procedure SetThreadCacheSize(Value: Integer);
    procedure SetOnThreadStart(Value: TThreadNotifyEvent);
    procedure SetOnThreadEnd(Value: TThreadNotifyEvent);
    procedure SetOnClientEvent(Index: Integer; Value: TSocketNotifyEvent);
    procedure SetOnClientError(Value: TSocketErrorEvent);
    
    property ServerType: TServerType read GetServerType write SetServerType;
    property ThreadCacheSize: Integer read GetThreadCacheSize write SetThreadCacheSize;
    property OnGetThread: TGetThreadEvent read GetGetThreadEvent write SetGetThreadEvent;
    property OnGetSocket: TGetSocketEvent read GetGetSocketEvent write SetGetSocketEvent;
    property OnThreadStart: TThreadNotifyEvent read GetOnThreadStart write SetOnThreadStart;
    property OnThreadEnd: TThreadNotifyEvent read GetOnThreadEnd write SetOnThreadEnd;
    property OnClientConnect: TSocketNotifyEvent index 2 read GetOnClientEvent write SetOnClientEvent;
    property OnClientDisconnect: TSocketNotifyEvent index 3 read GetOnClientEvent write SetOnClientEvent;
    property OnClientRead: TSocketNotifyEvent index 0 read GetOnClientEvent write SetOnClientEvent;
    property OnClientWrite: TSocketNotifyEvent index 1 read GetOnClientEvent write SetOnClientEvent;
    property OnClientError: TSocketErrorEvent read GetOnClientError write SetOnClientError;
  public
    destructor Destroy; override;
  end;

  TServerSocket = class(TCustomServerSocket)
  public
    constructor Create(AOwner: TComponent); override;
    property Socket: TServerWinSocket read FServerSocket;
  published
    property Active;
    property Port;
    property Service;
    property ServerType;
    property ThreadCacheSize default 10;
    property OnListen;
    property OnAccept;
    property OnGetThread;
    property OnGetSocket;
    property OnThreadStart;
    property OnThreadEnd;
    property OnClientConnect;
    property OnClientDisconnect;
    property OnClientRead;
    property OnClientWrite;
    property OnClientError;
  end;

  TSocketErrorProc = procedure (ErrorCode: Integer);

function SetErrorProc(ErrorProc: TSocketErrorProc): TSocketErrorProc;

implementation

uses RTLConsts;

//threadvar关键字的作用是：该变量在每个线程中有一份
threadvar
  SocketErrorProc: TSocketErrorProc;

var
  WSAData: TWSAData;

//设置Socket错误处理方法
function SetErrorProc(ErrorProc: TSocketErrorProc): TSocketErrorProc;
begin
  Result := SocketErrorProc;
  SocketErrorProc := ErrorProc;
end;

//检查Socket的结果
function CheckSocketResult(ResultCode: Integer; const Op: string): Integer;
begin
  if ResultCode <> 0 then
  begin
    Result := WSAGetLastError;      //获取最近一次的Socket错误代码
    //WSAEWOULDBLOCK（10035）的意思是Output Buffer已经满了，无法再写入数据
    //确切的说它其实不算是个错误，出现这种异常的绝大部分时候其实都不存在Output Buffer已满的情况，而是出于一种“忙”的状态
    //而这种“忙”的状态还很大程度上是由于接收方造成的
    //意思就是你要发送的对象，对方收的没有你发的快，或对方的接收缓冲区已被填满，所以就返回你一个“忙”的标志
    //这时候你再发送多少数据都没有任何意义，所以你的系统就抛出个 WSAEWOULDBLOCK异常通知你，叫你别瞎忙活了
    //那么，针对WSAEWOULDBLOCK应该怎么办呢？网上有很多朋友的做法是遇到这种情况就Sleep一段时间，一般短暂停顿后Output Buffer就空出来了，那就可以继续发送了
    //不过推荐另外的方法：根据MSDN文档所示，当出现WSAEWOULDBLOCK异常后直到空出Output Buffer时，系统会发送一个FD_WRITE给发送方
    //我们完全可以在等到FD_WRITE消息后再重新发送异常开始的数据包即可（该包需要全部重新发送）
    if Result <> WSAEWOULDBLOCK then
      if Assigned(SocketErrorProc) then
        SocketErrorProc(Result)
      else raise ESocketError.CreateResFmt(@sWindowsSocketError,
        [SysErrorMessage(Result), Result, Op]);
  end else Result := 0;
end;

//Windows网络编程中，最开始必须先使用WSAStartup
procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise ESocketError.CreateResFmt(@sWindowsSocketError,
      [SysErrorMessage(ErrorCode), ErrorCode, 'WSAStartup']);
end;

//与WSAStartup对应的，在网络程序退出时需要调用WSACleanup
procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise ESocketError.CreateResFmt(@sWindowsSocketError,
      [SysErrorMessage(ErrorCode), ErrorCode, 'WSACleanup']);
end;

{ TCustomWinSocket }
constructor TCustomWinSocket.Create(ASocket: TSocket);
begin
  inherited Create;
  Startup;                                                //WSAStartup
  FSocketLock := TCriticalSection.Create;                 //
  FASyncStyles := [asRead, asWrite, asConnect, asClose];  //读、写、连接、关闭时异步
  FSocket := ASocket;                                     //设置Windows原生的Socket
  FAddr.sin_family := PF_INET;
  FAddr.sin_addr.s_addr := INADDR_ANY;
  FAddr.sin_port := 0;
  FConnected := FSocket <> INVALID_SOCKET;
end;

destructor TCustomWinSocket.Destroy;
begin
  FOnSocketEvent := nil;  { disable events }
  if FConnected and (FSocket <> INVALID_SOCKET) then
    Disconnect(FSocket);
  if FHandle <> 0 then DeallocateHWnd(FHandle);
  FSocketLock.Free;
  Cleanup;                                                //WSACleanup
  FreeMem(FGetHostData);                                  //对于结构体在释放时，建议做一个置nil的处理
  FGetHostData := nil;
  inherited Destroy;
end;

//接收客户端连接，因为TCustomWinSocket会被客户端Socket、服务端Socket、服务端的客户端Socket
//而只有服务端Socket才会Accept，所以这里面Accept没有在父类中实现，具体会在服务端Socket中实现
procedure TCustomWinSocket.Accept(Socket: TSocket);
begin
end;

//异步的方式初始化Socket
procedure TCustomWinSocket.AsyncInitSocket(const Name, Address, Service: string; Port: Word; QueueSize: Integer; Client: Boolean);
var
  ErrorCode: Integer;
begin
  try
    case FLookupState of
      lsIdle:
        begin
          if not Client then
          begin
            FLookupState := lsLookupAddress;      //TLookupState = (lsIdle, lsLookupAddress, lsLookupService);
            FAddr.sin_addr.S_addr := INADDR_ANY;
          end else if Name <> '' then
          begin
            if FGetHostData = nil then
              FGetHostData := AllocMem(MAXGETHOSTSTRUCT);

            //WSAAsyncGetHostByName函数是gethostbyname()的异步版本，是用来获取对应于一个主机名的主机名称和地址信息
            FLookupHandle := WSAAsyncGetHostByName(Handle, CM_LOOKUPCOMPLETE,
              PChar(Name), FGetHostData, MAXGETHOSTSTRUCT);
            CheckSocketResult(Ord(FLookupHandle = 0), 'WSAASyncGetHostByName');
            FService := Service;
            FPort := Port;
            FQueueSize := QueueSize;            //监听队列大小 
            FClient := Client;                  //该Socket是不是客户端Socket
            FLookupState := lsLookupAddress;    //？？Lookup到底是啥
            Exit;
          end else if Address <> '' then
          begin
            FLookupState := lsLookupAddress;
            FAddr.sin_addr.S_addr := inet_addr(PChar(Address));
          end else
          begin
            ErrorCode := 1110;
            Error(Self, eeLookup, ErrorCode);
            Disconnect(FSocket);
            if ErrorCode <> 0 then
              raise ESocketError.CreateRes(@sNoAddress);
            Exit;
          end;
        end;
      lsLookupAddress:
        begin
          if Service <> '' then
          begin
            if FGetHostData = nil then
              FGetHostData := AllocMem(MAXGETHOSTSTRUCT);
              
            //WSAASyncGetServByName是getservbyname()的异步版本，是用来获取对应于一个服务名的服务信息
            FLookupHandle := WSAASyncGetServByName(Handle, CM_LOOKUPCOMPLETE,
              PChar(Service), 'tcp' , FGetHostData, MAXGETHOSTSTRUCT);
            CheckSocketResult(Ord(FLookupHandle = 0), 'WSAASyncGetServByName');
            FLookupState := lsLookupService;
            Exit;
          end else
          begin
            FLookupState := lsLookupService;
            FAddr.sin_port := htons(Port);
          end;
        end;
      lsLookupService:
        begin
          FLookupState := lsIdle;
          if Client then
            DoOpen
          else DoListen(QueueSize);
        end;
    end;

    //函数调用函数自己
    if FLookupState <> lsIdle then
      ASyncInitSocket(Name, Address, Service, Port, QueueSize, Client);
  except
    Disconnect(FSocket);
    raise;
  end;
end;

//关闭Socket，也就是断开连接
procedure TCustomWinSocket.Close;
begin
  Disconnect(FSocket);
end;

//发起连接，因为TCustomWinSocket会被客户端Socket、服务端Socket、服务端的客户端Socket
//而只有客户端Socket才会Connect，所以这里面Connect没有在父类中实现，具体会在服务端Socket中实现
procedure TCustomWinSocket.Connect(Socket: TSocket);
begin
end;

procedure TCustomWinSocket.Lock;
begin
  FSocketLock.Enter;
end;

procedure TCustomWinSocket.Unlock;
begin
  FSocketLock.Leave;
end;

{ TCMSocketMessage = record
    Msg: Cardinal;
    Socket: TSocket;
    SelectEvent: Word;
    SelectError: Word;
    Result: Longint;
  end;}
procedure TCustomWinSocket.CMSocketMessage(var Message: TCMSocketMessage);

  function CheckError: Boolean;
  var
    ErrorEvent: TErrorEvent;
    ErrorCode: Integer;
  begin
    if Message.SelectError <> 0 then
    begin
      Result := False;
      ErrorCode := Message.SelectError;
      case Message.SelectEvent of
        FD_CONNECT: ErrorEvent := eeConnect;
        FD_CLOSE: ErrorEvent := eeDisconnect;
        FD_READ: ErrorEvent := eeReceive;
        FD_WRITE: ErrorEvent := eeSend;
        FD_ACCEPT: ErrorEvent := eeAccept;
      else
        ErrorEvent := eeGeneral;
      end;
      Error(Self, ErrorEvent, ErrorCode);
      if ErrorCode <> 0 then
        raise ESocketError.CreateResFmt(@sASyncSocketError, [ErrorCode]);
    end else Result := True;
  end;

begin
  with Message do
    if CheckError then
      case SelectEvent of
        FD_CONNECT: Connect(Socket);         //调用Connect方法，响应FD_CONNECT
        FD_CLOSE: Disconnect(Socket);
        FD_READ: Read(Socket);
        FD_WRITE: Write(Socket);
        FD_ACCEPT: Accept(Socket);
      end;
{WSAAsyncSelect模型允许应用程序以Windows消息的方式接收网络事件通知。许多对性能要求不高的网络应用程序都采用WSAAsyncSelect模型
WSAAsyncSelect自动将套接字设置为非阻塞模式，并且为套接字绑定一个窗口句柄，当有网络事件发生时，便向这个窗口发送消息
int WSAAsyncSelect(
  SOCKET s,     //需要设置的套接字句柄
  HWND, hWnd,   //指定一个窗口句柄，套接字的通知消息将被发到此窗口中
  u_int wMsg,   //网络事件到来的ID，可以在WM_USER以上数值中任意指定一个值
  long lEvent   //指定哪些通知码需要发送
                //FD_READ：可以读套接字
                //FD_WRITE：可以写套接字
                //FD_ACCEPT：监听套接字有连接接入
                //FD_CONNECT：如果套接字连接对方主机
                //FD_CLOSE：检测到套接字对应的连接被关闭)
回传过来的消息类型与注册的wMsg相同，wParam等于套接字句柄
lParam通过WSAGETSELECTEVENT转义后就是FD_READ，FD_WRITE，FD_ACCEPT，FD_CONNET，FD_CLOSE}
end;

//procedure CMDeferFree(var Message); message CM_DEFERFREE;
//响应CM_DEFERFREE消息
procedure TCustomWinSocket.CMDeferFree(var Message);
begin
  Free;
end;

//PostMessage异步发送消息
procedure TCustomWinSocket.DeferFree;
begin
  if FHandle <> 0 then PostMessage(FHandle, CM_DEFERFREE, 0, 0);
end;

//
procedure TCustomWinSocket.DoSetAsyncStyles;
var
  Msg: Integer;
  Wnd: HWnd;
  Blocking: Longint;
begin
  Msg := 0;
  Wnd := 0;
  if FAsyncStyles <> [] then
  begin
    Msg := CM_SOCKETMESSAGE;   //CM_SOCKETMESSAGE = WM_USER + $0001;
    Wnd := Handle;
  end;
{WSAAsyncSelect模型允许应用程序以Windows消息的方式接收网络事件通知。许多对性能要求不高的网络应用程序都采用WSAAsyncSelect模型
WSAAsyncSelect自动将套接字设置为非阻塞模式，并且为套接字绑定一个窗口句柄，当有网络事件发生时，便向这个窗口发送消息
int WSAAsyncSelect(
  SOCKET s,     //需要设置的套接字句柄
  HWND, hWnd,   //指定一个窗口句柄，套接字的通知消息将被发到此窗口中
  u_int wMsg,   //网络事件到来的ID，可以在WM_USER以上数值中任意指定一个值
  long lEvent   //指定哪些通知码需要发送
                //FD_READ：可以读套接字
                //FD_WRITE：可以写套接字
                //FD_ACCEPT：监听套接字有连接接入
                //FD_CONNECT：如果套接字连接对方主机
                //FD_CLOSE：检测到套接字对应的连接被关闭
)
回传过来的消息类型与注册的wMsg相同，wParam等于套接字句柄
lParam通过WSAGETSELECTEVENT转义后就是FD_READ，FD_WRITE，FD_ACCEPT，FD_CONNET，FD_CLOSE}

  //在FSocket上注册CM_SOCKETMESSAGE消息
  //当有消息的时候通过消息CM_SOCKETMESSAGE通知Wnd对应的窗体，对应的窗体Wnd上注册CM_SOCKETMESSAGE消息去响应即可
  //注册的通知码是FAsyncStyles集合
  WSAAsyncSelect(FSocket, Wnd, Msg, Longint(Byte(FAsyncStyles)));

  //如果异步方式集合为空，则使用ioctlsocket进行下面的设置Socket为阻塞模式
  if FASyncStyles = [] then
  begin
    Blocking := 0;
    {ioctlsocket(
      SOCKET s,         //套接字描述符
      long cmd,         //对套接字的操作命令
      u_long FAR* argp  //指向cmd命令所带参数的指针
    )
    本函数可用于任一状态的任意套接字，它用于获取与套接字相关的操作参数，而与具体协议或通讯子系统无关
    命令：FIONBIO：允许或禁止套接字S的非阻塞模式。argp指向一个无符号长整型
    如允许非阻塞模式则非零，如禁止非阻塞模式则为零
    这里是Blocking为0，表示设置为阻塞模式}
    ioctlsocket(FSocket, FIONBIO, Blocking);
  end;
end;

procedure TCustomWinSocket.DoListen(QueueSize: Integer);
begin
  //将Socket绑定到对应的端口上，并且检查返回值
  CheckSocketResult(bind(FSocket, FAddr, SizeOf(FAddr)), 'bind');

  //设置异步模式：是否阻塞、如果非阻塞则通过WSAAsyncSelect去注册响应哪些消息
  DoSetASyncStyles;

  //SOMAXCONN       = 5;
  if QueueSize > SOMAXCONN then QueueSize := SOMAXCONN;

  //回调处理seListen事件函数
  Event(Self, seListen);

  //监听FSocket套接字
  //listen的第二个参数是等待连接队列的最大长度
  //比如说，将其设置为10，当有15个连接请求的时候，前10个连接请求被放置到请求队列中，后面5个请求被拒绝
  CheckSocketResult(Winsock.listen(FSocket, QueueSize), 'listen');
  FLookupState := lsIdle;
  FConnected := True;
end;

procedure TCustomWinSocket.DoOpen;
begin
  //设置异步模式：是否阻塞、如果非阻塞则通过WSAAsyncSelect去注册响应哪些消息
  DoSetASyncStyles;

  //回调处理seConnecting事件的函数
  Event(Self, seConnecting);

  //调用connect发起连接，并且检查返回的结果值！
  CheckSocketResult(WinSock.connect(FSocket, FAddr, SizeOf(FAddr)), 'connect');

  FLookupState := lsIdle;

  //如果连接属性不在异步方式集合中，也就是连接设置为阻塞模式
  if not (asConnect in FAsyncStyles) then
  begin
    //连接成功的情况下，回调处理seConnect事件的函数
    FConnected := FSocket <> INVALID_SOCKET;
    Event(Self, seConnect);
  end;
end;

{WndProc函数是下面这样子的
procedure TCustomWinSocket.WndProc(var Message: TMessage);
begin
  try
    Dispatch(Message);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;}
function TCustomWinSocket.GetHandle: HWnd;
begin
  if FHandle = 0 then
    {AllocateHWnd创建一个与某窗口化空间无关且执行Method参数指定窗口过程窗口
    一般用于创建非可视化窗口，即其响应未出现在用户界面上的消息}
    FHandle := AllocateHwnd(WndProc);
  Result := FHandle;
end;

//获取本机的IP 地址
function TCustomWinSocket.GetLocalAddress: string;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
begin
  Lock;
  try
    Result := '';
    if FSocket = INVALID_SOCKET then Exit;
    Size := SizeOf(SockAddrIn);
    //getsockname()函数用于获取一个套接字的名字，它用于一个已绑定或已连接的套接字
    //本地地址将被返回
    if getsockname(FSocket, SockAddrIn, Size) = 0 then
      //本函数将一个用in参数所表示的Internet地址结构转换成以“.” 间隔的诸如“a.b.c.d”的字符串形式
      Result := inet_ntoa(SockAddrIn.sin_addr);
  finally
    Unlock;
  end;
end;

//获取本机Host名称
function TCustomWinSocket.GetLocalHost: string;
var
  LocalName: array[0..255] of Char;
begin
  Lock;
  try
    Result := '';
    if FSocket = INVALID_SOCKET then Exit;
    //gethostname，返回本地主机的标准主机名
    if gethostname(LocalName, SizeOf(LocalName)) = 0 then
      Result := LocalName;
  finally
    Unlock;
  end;
end;

//获取本地主机的IP地址
function TCustomWinSocket.GetLocalPort: Integer;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
begin
  Lock;
  try
    Result := -1;
    if FSocket = INVALID_SOCKET then Exit;
    Size := SizeOf(SockAddrIn);
    if getsockname(FSocket, SockAddrIn, Size) = 0 then
      //将一个16位网络字节序转换为主机字节序
      Result := ntohs(SockAddrIn.sin_port);
  finally
    Unlock;
  end;
end;

//获取对方机器的Host名称
function TCustomWinSocket.GetRemoteHost: string;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
  HostEnt: PHostEnt;
begin
  Lock;
  try
    Result := '';
    if not FConnected then Exit;
    Size := SizeOf(SockAddrIn);
    //getpeername：获取socket的对方地址（SockAddrIn中包括IP、端口信息）
    CheckSocketResult(getpeername(FSocket, SockAddrIn, Size), 'getpeername');
    //gethostbyaddr：根据地址获取对应的Host名称信息
    HostEnt := gethostbyaddr(@SockAddrIn.sin_addr.s_addr, 4, PF_INET);
    if HostEnt <> nil then Result := HostEnt.h_name;
  finally
    Unlock;
  end;
end;

//获取对方的IP地址
function TCustomWinSocket.GetRemoteAddress: string;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
begin
  Lock;
  try
    Result := '';
    if not FConnected then Exit;
    Size := SizeOf(SockAddrIn);
    CheckSocketResult(getpeername(FSocket, SockAddrIn, Size), 'getpeername');
    Result := inet_ntoa(SockAddrIn.sin_addr);
  finally
    Unlock;
  end;
end;

//获取对方的端口信息
function TCustomWinSocket.GetRemotePort: Integer;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
begin
  Lock;
  try
    Result := 0;
    if not FConnected then Exit;
    Size := SizeOf(SockAddrIn);
    CheckSocketResult(getpeername(FSocket, SockAddrIn, Size), 'getpeername');
    Result := ntohs(SockAddrIn.sin_port);
  finally
    Unlock;
  end;
end;

//获取对方的IP、端口信息
function TCustomWinSocket.GetRemoteAddr: TSockAddrIn;
var
  Size: Integer;
begin
  Lock;
  try
    FillChar(Result, SizeOf(Result), 0);
    if not FConnected then Exit;
    Size := SizeOf(Result);
    if getpeername(FSocket, Result, Size) <> 0 then
      FillChar(Result, SizeOf(Result), 0);
  finally
    Unlock;
  end;
end;

//Lookup到底是啥？
function TCustomWinSocket.LookupName(const Name: string): TInAddr;
var
  HostEnt: PHostEnt;
  InAddr: TInAddr;
begin
  //gethostbyname：返回对应于给定主机名的包含主机名和地址信息的hostent结构指针
  HostEnt := gethostbyname(PChar(Name));
  FillChar(InAddr, SizeOf(InAddr), 0);
  if HostEnt <> nil then
  begin
    with InAddr, HostEnt^ do
    begin
      S_un_b.s_b1 := h_addr^[0];
      S_un_b.s_b2 := h_addr^[1];
      S_un_b.s_b3 := h_addr^[2];
      S_un_b.s_b4 := h_addr^[3];
    end;
  end;
  Result := InAddr;
end;

function TCustomWinSocket.LookupService(const Service: string): Integer;
var
  ServEnt: PServEnt;
begin
  //getservbyname，返回与给定服务名对应的包含名字和服务号信息的servent结构指针
  ServEnt := getservbyname(PChar(Service), 'tcp');
  if ServEnt <> nil then
    Result := ntohs(ServEnt.s_port)
  else Result := 0;
end;

//初始化Socket的地址信息
{sockaddr_in = record
    case Integer of
      0: (sin_family: u_short;             //协议族，在Socket编程中只能是AF_INET
          sin_port: u_short;               //存储端口号（使用网络字节序）
          sin_addr: TInAddr;               //存储IP地址，使用id_addr这个数据结构
          sin_zero: array[0..7] of Char);  //是为了让sockaddr与sockaddr_in两个数据结构保持大小相同而保留的空字节
      1: (sa_family: u_short;              //
          sa_data: array[0..13] of Char)   //
  end;
  TSockAddrIn = sockaddr_in;}
function TCustomWinSocket.InitSocket(const Name, Address, Service: string; Port: Word; Client: Boolean): TSockAddrIn;
begin
  Result.sin_family := PF_INET;
  if Name <> '' then
    Result.sin_addr := LookupName(name)
  else if Address <> '' then
    Result.sin_addr.s_addr := inet_addr(PChar(Address))
  else if not Client then
    Result.sin_addr.s_addr := INADDR_ANY
  else raise ESocketError.CreateRes(@sNoAddress);
  
  if Service <> '' then
    Result.sin_port := htons(LookupService(Service))
  else
    Result.sin_port := htons(Port);
end;

procedure TCustomWinSocket.Listen(const Name, Address, Service: string; Port: Word; QueueSize: Integer; Block: Boolean);
begin
  //sCannotListenOnOpen = 'Can''t listen on an open socket';
  if FConnected then raise ESocketError.CreateRes(@sCannotListenOnOpen);

  //创建一个Socket
  FSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  
  //sCannotCreateSocket = 'Can''t create new socket';
  if FSocket = INVALID_SOCKET then raise ESocketError.CreateRes(@sCannotCreateSocket);

  try
    //回调seLookup事件处理函数
    Event(Self, seLookUp);
    if Block then
    begin
      //如果是阻塞的模式，则调用InitSocket方法初始化Socket的地址信息
      FAddr := InitSocket(Name, Address, Service, Port, False);
      //然后调用listen方法
      DoListen(QueueSize);
    end else
    begin
      //如果是非阻塞模式，则调用AsyncInitSocket初始化Socket的地址信息
      //AsyncInitSocket这个方法比较复杂，目前还没有很懂
      AsyncInitSocket(Name, Address, Service, Port, QueueSize, False);
    end;
  except
    Disconnect(FSocket);
    raise;
  end;
end;

procedure TCustomWinSocket.Open(const Name, Address, Service: string; Port: Word; Block: Boolean);
begin
  //sSocketAlreadyOpen = 'Socket already open';
  if FConnected then raise ESocketError.CreateRes(@sSocketAlreadyOpen);

  //创建一个Socket
  FSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);

  //判断Socket是否创建成功
  if FSocket = INVALID_SOCKET then raise ESocketError.CreateRes(@sCannotCreateSocket);
  
  try
    //回调seLookup事件处理函数
    Event(Self, seLookUp);
    if Block then
    begin
      //如果是阻塞模式，则调用InitSocket初始化
      FAddr := InitSocket(Name, Address, Service, Port, True);
      //然后Open Socket
      DoOpen;
    end else
    begin
      //如果是非阻塞模式，则调用AsyncInitSocket初始化
      AsyncInitSocket(Name, Address, Service, Port, 0, True);
    end;
  except
    Disconnect(FSocket);
    raise;
  end;
end;

//断开连接
procedure TCustomWinSocket.Disconnect(Socket: TSocket);
begin
  Lock;
  try
    //WSACancelASyncRequest函数用于取消一次异步操作
    //该异步操作是以一个WSAAsyncGetXByY函数诸如WSAAsyncGetHostByName()启动的
    if FLookupHandle <> 0 then
      CheckSocketResult(WSACancelASyncRequest(FLookupHandle), 'WSACancelASyncRequest');

    FLookupHandle := 0;
    if (Socket = INVALID_SOCKET) or (Socket <> FSocket) then exit;

    //回调seDisconnect事件处理函数
    Event(Self, seDisconnect);

    //关闭一个Socket，并且检查返回值
    CheckSocketResult(closesocket(FSocket), 'closesocket');

    FSocket := INVALID_SOCKET;
    FAddr.sin_family := PF_INET;
    FAddr.sin_addr.s_addr := INADDR_ANY;
    FAddr.sin_port := 0;
    FConnected := False;
    FreeAndNil(FSendStream);
  finally
    Unlock;
  end;
end;

{CallWindowProc函数是将消息传送给指定的窗口过程
CallWindowProc的参数
  pPrevWndFunc：指向前一个窗口过程的指针。如果该值是通过调用GetWindowLong函数，并将该函数中的nlndex参数设为GWL_WNDPROC或DWL_DLGPROC而得到的，那么它实际上要么是窗口或者对话框的地址，要么就是代表该地址的句柄。
  hWnd：指向接收消息的窗口过程的句柄。
  Msg：指定消息类型。
  wParam：指定其余的、消息特定的信息。该参数的内容与Msg参数值有关。
  IParam：指定其余的、消息特定的信息。该参数的内容与Msg参数值有关。}
procedure TCustomWinSocket.DefaultHandler(var Message);
begin
  with TMessage(Message) do
    if FHandle <> 0 then
      Result := CallWindowProc(@DefWindowProc, FHandle, Msg, wParam, lParam);
end;

//回调不同的事件处理函数
//TSocketEvent = (seLookup, seConnecting, seConnect, seDisconnect, seListen, seAccept, seWrite, seRead);
procedure TCustomWinSocket.Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
begin
  if Assigned(FOnSocketEvent) then FOnSocketEvent(Self, Socket, SocketEvent);
end;

//回调不同的错误处理函数（ErrorEvent）
//TErrorEvent = (eeGeneral, eeSend, eeReceive, eeConnect, eeDisconnect, eeAccept, eeLookup);
procedure TCustomWinSocket.Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  if Assigned(FOnErrorEvent) then FOnErrorEvent(Self, Socket, ErrorEvent, ErrorCode);
end;

//发送文本信息
function TCustomWinSocket.SendText(const s: string): Integer;
begin
  Result := SendBuf(Pointer(S)^, Length(S));
end;

function TCustomWinSocket.SendStreamPiece: Boolean;
var
  Buffer: array[0..4095] of Byte;
  StartPos: Integer;
  AmountInBuf: Integer;
  AmountSent: Integer;
  ErrorCode: Integer;

  procedure DropStream;
  begin
    if FDropAfterSend then Disconnect(FSocket);
    FDropAfterSend := False;
    FSendStream.Free;
    FSendStream := nil;
  end;

begin
  Lock;
  try
    Result := False;
    if FSendStream <> nil then
    begin
      //如果Socket非法，或者没有建立起连接，则直接退出
      if (FSocket = INVALID_SOCKET) or (not FConnected) then exit;

      while True do
      begin
        StartPos := FSendStream.Position;
        //Buffer: array[0..4095] of Byte;
        //所以这里是每次读4096字节的数据
        AmountInBuf := FSendStream.Read(Buffer, SizeOf(Buffer));
        if AmountInBuf > 0 then
        begin
          //调用SOCKAPI，send发送数据
          AmountSent := send(FSocket, Buffer, AmountInBuf, 0);

          //判断返回值
          if AmountSent = SOCKET_ERROR then
          begin
            ErrorCode := WSAGetLastError;
            //WSAEWOULDBLOCK（10035）的意思是Output Buffer已经满了，无法再写入数据
            if ErrorCode <> WSAEWOULDBLOCK then
            begin
              //回调发送时出现错误对应的处理函数
              Error(Self, eeSend, ErrorCode);

              //断开连接
              Disconnect(FSocket);

              //释放流对象
              DropStream;
              if FAsyncStyles <> [] then Abort;
              Break;
            end else
            begin
              FSendStream.Position := StartPos;
              Break;
            end;
          end else if AmountInBuf > AmountSent then
            FSendStream.Position := StartPos + AmountSent
          else if FSendStream.Position = FSendStream.Size then
          begin
            DropStream;
            Break;
          end;
        end else
        begin
          DropStream;
          Break;
        end;
      end;
      Result := True;
    end;
  finally
    Unlock;
  end;
end;

//发送AStream流中的数据
function TCustomWinSocket.SendStream(AStream: TStream): Boolean;
begin
  Result := False;
  if FSendStream = nil then
  begin
    FSendStream := AStream;
    Result := SendStreamPiece;
  end;
end;

//？？
function TCustomWinSocket.SendStreamThenDrop(AStream: TStream): Boolean;
begin
  FDropAfterSend := True;
  Result := SendStream(AStream);
  if not Result then FDropAfterSend := False;
end;

//发送二进制数据
//在 http://www.xumenger.com/Delphi-binary-socket-20161222/ 中有详细的说明
function TCustomWinSocket.SendBuf(var Buf; Count: Integer): Integer;
var
  ErrorCode: Integer;
begin
  Lock;
  try
    Result := 0;
    if not FConnected then Exit;

    //调用send方法发送数据
    Result := send(FSocket, Buf, Count, 0);
    if Result = SOCKET_ERROR then
    begin
      ErrorCode := WSAGetLastError;

      //如果不是缓冲区满了导致的错误，则回调对应的发送错误处理函数
      if (ErrorCode <> WSAEWOULDBLOCK) then
      begin
        Error(Self, eeSend, ErrorCode);
        Disconnect(FSocket);
        if ErrorCode <> 0 then
          raise ESocketError.CreateResFmt(@sWindowsSocketError,
            [SysErrorMessage(ErrorCode), ErrorCode, 'send']);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TCustomWinSocket.SetAsyncStyles(Value: TASyncStyles);
begin
  if Value <> FASyncStyles then
  begin
    FASyncStyles := Value;
    if FSocket <> INVALID_SOCKET then
      DoSetAsyncStyles;
  end;
end;

procedure TCustomWinSocket.Read(Socket: TSocket);
begin
  if (FSocket = INVALID_SOCKET) or (Socket <> FSocket) then Exit;

  //回调开发者实现的seRead事件处理方法
  //一般就是我们在开发中实现的OnRead方法
  Event(Self, seRead);
end;

//从接收缓冲区中读取Count字节的数据
function TCustomWinSocket.ReceiveBuf(var Buf; Count: Integer): Integer;
var
  ErrorCode: Integer;
begin
  Lock;
  try
    Result := 0;

    //当Conut为-1时，是ReceiveLength函数来获取接收缓冲区中目前已有的数量
    if (Count = -1) and FConnected then
      //ioctlsocket是控制套接字的模式
      //第二个参数FIONREAD：确定套接字自动读入的数据量
      //当第二参数为FIONREAD时，第三个参数存有ioctlsocket的返回值
      //如果s是SOCKET_STREAM类型，则FIONREAD返回在一次recv()中所接收的所有数据量。这通常与套接口中排队的数据总量相同
      //如果S是SOCK_DGRAM 型，则FIONREAD返回套接口上排队的第一个数据报大小
      ioctlsocket(FSocket, FIONREAD, Longint(Result))
    else begin
      if not FConnected then Exit;

      //调用recv方法，从接收缓冲区中读取数据
      Result := recv(FSocket, Buf, Count, 0);

      //判断recv的返回值
      if Result = SOCKET_ERROR then
      begin
        ErrorCode := WSAGetLastError;
        //WSAEWOULDBLOCK（10035）的意思是Output Buffer已经满了，无法再写入数据
        if ErrorCode <> WSAEWOULDBLOCK then
        begin
          //回调eeReveive对应的处理函数
          Error(Self, eeReceive, ErrorCode);
          Disconnect(FSocket);
          if ErrorCode <> 0 then
            raise ESocketError.CreateResFmt(@sWindowsSocketError,
              [SysErrorMessage(ErrorCode), ErrorCode, 'recv']);
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

//返回当前缓冲区中的数据总量
function TCustomWinSocket.ReceiveLength: Integer;
begin
  Result := ReceiveBuf(Pointer(nil)^, -1);
end;

//读取当前接收缓冲区中所有的数据，并且以文本字符串的方式返回
function TCustomWinSocket.ReceiveText: string;
begin
  //设置字符串Result的长度为当前接收缓冲区中数据里大小
  SetLength(Result, ReceiveBuf(Pointer(nil)^, -1));
  //从当前的接收缓冲区中读取相应数量的数据
  SetLength(Result, ReceiveBuf(Pointer(Result)^, Length(Result)));
end;

procedure TCustomWinSocket.WndProc(var Message: TMessage);
begin
  try
    //负责将特定的消息分发给合适的消息处理函数
    Dispatch(Message);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;

procedure TCustomWinSocket.Write(Socket: TSocket);
begin
  if (FSocket = INVALID_SOCKET) or (Socket <> FSocket) then Exit;
  if not SendStreamPiece then Event(Self, seWrite);
end;

//处理CM_LOOKUPCOMPLETE消息
//Lookup到底是代表啥？
procedure TCustomWinSocket.CMLookupComplete(var Message: TCMLookupComplete);
var
  ErrorCode: Integer;
begin
  if Message.LookupHandle = FLookupHandle then
  begin
    FLookupHandle := 0;
    if Message.AsyncError <> 0 then
    begin
      ErrorCode := Message.AsyncError;
      Error(Self, eeLookup, ErrorCode);
      Disconnect(FSocket);
      if ErrorCode <> 0 then
        raise ESocketError.CreateResFmt(@sWindowsSocketError,
          [SysErrorMessage(Message.AsyncError), Message.ASyncError, 'ASync Lookup']);
      Exit;
    end;
    if FLookupState = lsLookupAddress then
    begin
      FAddr.sin_addr.S_addr := Integer(Pointer(PHostEnt(FGetHostData).h_addr^)^);
      ASyncInitSocket('', '', FService, FPort, FQueueSize, FClient);
    end else if FLookupState = lsLookupService then
    begin
      FAddr.sin_port := PServEnt(FGetHostData).s_port;
      FPort := 0;
      FService := '';
      ASyncInitSocket('', '', '', 0, FQueueSize, FClient);
    end;
  end;
end;

{ TClientWinSocket }
//客户端的Socket
procedure TClientWinSocket.Connect(Socket: TSocket);
begin
  FConnected := True;
  Event(Self, seConnect);
end;

//设置客户端阻塞类型
procedure TClientWinSocket.SetClientType(Value: TClientType);
begin
  if Value <> FClientType then
    if not FConnected then
    begin
      FClientType := Value;
      if FClientType = ctBlocking then
      begin
        //如果是阻塞模式，则异步方式集合为空
        ASyncStyles := []
      end
      else
      begin
        //如果是非阻塞模式，则读、写、连接、关闭事件都是异步的
        ASyncStyles := [asRead, asWrite, asConnect, asClose];
      end
    end else raise ESocketError.CreateRes(@sCantChangeWhileActive);
end;

{ TServerClientWinsocket }
//服务端对应客户端连接的Socket

constructor TServerClientWinSocket.Create(Socket: TSocket; ServerWinSocket: TServerWinSocket);
begin
  FServerWinSocket := ServerWinSocket;
  if Assigned(FServerWinSocket) then
  begin
    FServerWinSocket.AddClient(Self);           //将该Socket添加到服务端的连接管理链表中

    //如果服务端Socket是非阻塞模式
    if FServerWinSocket.AsyncStyles <> [] then
    begin
      OnSocketEvent := FServerWinSocket.ClientEvent;
      OnErrorEvent := FServerWinSocket.ClientError;
    end;
  end;
  inherited Create(Socket);
  if FServerWinSocket.ASyncStyles <> [] then
  begin
    DoSetAsyncStyles;
  end;
  if FConnected then Event(Self, seConnect);
end;

destructor TServerClientWinSocket.Destroy;
begin
  if Assigned(FServerWinSocket) then
    FServerWinSocket.RemoveClient(Self);      //从服务端的管理链表中删除该Socket
  inherited Destroy;
end;

{ TServerWinSocket }
//服务端监听Socket
constructor TServerWinSocket.Create(ASocket: TSocket);
begin
  FConnections := TList.Create;          //创建客户端连接管理链表
  FActiveThreads := TList.Create;        //创建客户端连接对应的处理线程管理链表
  FListLock := TCriticalSection.Create;
  inherited Create(ASocket);
  FAsyncStyles := [asAccept];            //将接收客户端连接事件设置为异步方式
end;

destructor TServerWinSocket.Destroy;
begin
  inherited Destroy;
  FConnections.Free;
  FActiveThreads.Free;
  FListLock.Free;
end;

//往连接管理链表中添加一个连接Socket
procedure TServerWinSocket.AddClient(AClient: TServerClientWinSocket);
begin
  FListLock.Enter;
  try
    if FConnections.IndexOf(AClient) < 0 then
      FConnections.Add(AClient);
  finally
    FListLock.Leave;
  end;
end;

//从连接管理链表中删除一个客户端连接Socket
procedure TServerWinSocket.RemoveClient(AClient: TServerClientWinSocket);
begin
  FListLock.Enter;
  try
    FConnections.Remove(AClient);
  finally
    FListLock.Leave;
  end;
end;

//往线程管理链表中添加一个客户端连接处理线程
procedure TServerWinSocket.AddThread(AThread: TServerClientThread);
begin
  FListLock.Enter;
  try
    if FActiveThreads.IndexOf(AThread) < 0 then
    begin
      FActiveThreads.Add(AThread);

      //判断当前线程管理链表中的线程个数
      if FActiveThreads.Count <= FThreadCacheSize then
        AThread.KeepInCache := True;
    end;
  finally
    FListLock.Leave;
  end;
end;

//从线程管理链表中删除一个线程
procedure TServerWinSocket.RemoveThread(AThread: TServerClientThread);
begin
  FListLock.Enter;
  try
    FActiveThreads.Remove(AThread);
  finally
    FListLock.Leave;
  end;
end;

//客户端事件处理函数
procedure TServerWinSocket.ClientEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
begin
  //如果是连接事件、Lookup事件、正在连接事件、监听事件则不处理
  case SocketEvent of
    seAccept,
    seLookup,
    seConnecting,
    seListen:
      begin end;

    seConnect: ClientConnect(Socket);       //如果是连接事件，则回调ClientConnect方法处理
    seDisconnect: ClientDisconnect(Socket); //同上的逻辑
    seRead: ClientRead(Socket);
    seWrite: ClientWrite(Socket);
  end;
end;

//客户端错误事件处理函数
procedure TServerWinSocket.ClientError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  ClientErrorEvent(Socket, ErrorEvent, ErrorCode);
end;

//获取当前连接管理链表中的对象个数
function TServerWinSocket.GetActiveConnections: Integer;
begin
  Result := FConnections.Count;
end;

//获取连接管理链表中的第Index个连接
function TServerWinSocket.GetConnections(Index: Integer): TCustomWinSocket;
begin
  Result := FConnections[Index];
end;

//获取当前线程管理链表中“活着”的线程的个数
function TServerWinSocket.GetActiveThreads: Integer;
var
  I: Integer;
begin
  FListLock.Enter;
  try
    Result := 0;
    for I := 0 to FActiveThreads.Count - 1 do
      if TServerClientThread(FActiveThreads[I]).ClientSocket <> nil then
        Inc(Result);
  finally
    FListLock.Leave;
  end;
end;

//获取当前线程管理链表中“死了”的线程的个数
function TServerWinSocket.GetIdleThreads: Integer;
var
  I: Integer;
begin
  FListLock.Enter;
  try
    Result := 0;
    for I := 0 to FActiveThreads.Count - 1 do
      if TServerClientThread(FActiveThreads[I]).ClientSocket = nil then
        Inc(Result);
  finally
    FListLock.Leave;
  end;
end;

//服务端接收客户端连接
procedure TServerWinSocket.Accept(Socket: TSocket);
var
  ClientSocket: TServerClientWinSocket;
  ClientWinSocket: TSocket;
  Addr: TSockAddrIn;
  Len: Integer;
  OldOpenType, NewOpenType: Integer;
begin
  Len := SizeOf(OldOpenType);
  //getsockopt() 函数用于获取任意类型、任意状态套接字的选项的当前值
  {int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t*optlen);
    sockfd：一个标识套接口的描述字。
    level：选项定义的层次。例如，支持的层次有SOL_SOCKET、IPPROTO_TCP。
    optname：需获取的套接口选项。
    optval：指针，指向存放所获得选项值的缓冲区。
    optlen：指针，指向optval缓冲区的长度值}
  if getsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, PChar(@OldOpenType),
    Len) = 0 then
  try
    if FServerType = stThreadBlocking then
    begin
      NewOpenType := SO_SYNCHRONOUS_NONALERT;
      //setsockopt()函数用于对任意类型、任意状态套接字设置选项
      setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, PChar(@NewOpenType), Len);
    end;
    Len := SizeOf(Addr);

    //接收客户端连接，为该客户端生成一个Socket
    ClientWinSocket := WinSock.accept(Socket, @Addr, @Len);
    
    if ClientWinSocket <> INVALID_SOCKET then
    begin
      //根据Socket创建一个TServerClientWinSocket对象
      ClientSocket := GetClientSocket(ClientWinSocket);
      if Assigned(FOnSocketEvent) then
        //回调seAccept事件处理函数
        FOnSocketEvent(Self, ClientSocket, seAccept);

      if FServerType = stThreadBlocking then
      begin
        //如果服务端是非阻塞模式，则为每个客户端获得一个专门的处理线程
        ClientSocket.ASyncStyles := [];
        GetServerThread(ClientSocket);
      end;
    end;
  finally
    Len := SizeOf(OldOpenType);
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, PChar(@OldOpenType), Len);
  end;
end;

//断开连接
procedure TServerWinSocket.Disconnect(Socket: TSocket);
var
  SaveCacheSize: Integer;
begin
  Lock;
  try
    SaveCacheSize := ThreadCacheSize;
    try
      ThreadCacheSize := 0;
      while FActiveThreads.Count > 0 do
        with TServerClientThread(FActiveThreads.Last) do
        begin
          FreeOnTerminate := False;
          Terminate;
          FEvent.SetEvent;
          if (ClientSocket <> nil) and ClientSocket.Connected then
            ClientSocket.Close;
          WaitFor;  
          Free;
        end;
      while FConnections.Count > 0 do
        TCustomWinSocket(FConnections.Last).Free;
      if FServerAcceptThread <> nil then
        FServerAcceptThread.Terminate;
      inherited Disconnect(Socket);
      FServerAcceptThread.Free;
      FServerAcceptThread := nil;
    finally
      ThreadCacheSize := SaveCacheSize;
    end;
  finally
    Unlock;
  end;
end;

//为TServerClientWinSocket创建一个对应的处理线程
function TServerWinSocket.DoCreateThread(ClientSocket: TServerClientWinSocket): TServerClientThread;
begin
  Result := TServerClientThread.Create(False, ClientSocket);
end;

//监听端口等待客户端连接
procedure TServerWinSocket.Listen(var Name, Address, Service: string; Port: Word;
  QueueSize: Integer);
begin
  inherited Listen(Name, Address, Service, Port, QueueSize, ServerType = stThreadBlocking);

  //如果是阻塞模式，则专门创建一个接受客户端连接的线程
  if FConnected and (ServerType = stThreadBlocking) then
    FServerAcceptThread := TServerAcceptThread.Create(False, Self);
end;

procedure TServerWinSocket.SetServerType(Value: TServerType);
begin
  if Value <> FServerType then
    if not FConnected then
    begin
      FServerType := Value;
      if FServerType = stThreadBlocking then
        ASyncStyles := []
      else ASyncStyles := [asAccept];
    end else raise ESocketError.CreateRes(@sCantChangeWhileActive);
end;

//设置线程池中允许的线程的最大个数
procedure TServerWinSocket.SetThreadCacheSize(Value: Integer);
var
  Start, I: Integer;
begin
  if Value <> FThreadCacheSize then
  begin
    if Value < FThreadCacheSize then
      Start := Value
    else Start := FThreadCacheSize;
    FThreadCacheSize := Value;
    FListLock.Enter;
    try
      for I := 0 to FActiveThreads.Count - 1 do
        with TServerClientThread(FActiveThreads[I]) do
          KeepInCache := I < Start;
    finally
      FListLock.Leave;
    end;
  end;
end;

//在服务端根据accept返回的Socket，创建一个表示客户端连接的TServerClientWinSocket对象
function TServerWinSocket.GetClientSocket(Socket: TSocket): TServerClientWinSocket;
begin
  Result := nil;
  if Assigned(FOnGetSocket) then FOnGetSocket(Self, Socket, Result);
  if Result = nil then
    Result := TServerClientWinSocket.Create(Socket, Self);
end;

//回调线程结束事件处理函数
procedure TServerWinSocket.ThreadEnd(AThread: TServerClientThread);
begin
  if Assigned(FOnThreadEnd) then FOnThreadEnd(Self, AThread);
end;

//回调线程开始事件处理函数
procedure TServerWinSocket.ThreadStart(AThread: TServerClientThread);
begin
  if Assigned(FOnThreadStart) then FOnThreadStart(Self, AThread);
end;

//为TServerClientWinSocket获得一个专门的线程
//如果当前线程管理链表中有“死了”的线程，则重新激活该线程去处理新的TServerClientWinSocket
//否则创建一个新的线程
function TServerWinSocket.GetServerThread(ClientSocket: TServerClientWinSocket): TServerClientThread;
var
  I: Integer;
begin
  Result := nil;
  FListLock.Enter;
  try
    for I := 0 to FActiveThreads.Count - 1 do
      if TServerClientThread(FActiveThreads[I]).ClientSocket = nil then
      begin
        Result := FActiveThreads[I];
        Result.ReActivate(ClientSocket);
        Break;
      end;
  finally
    FListLock.Leave;
  end;
  if Result = nil then
  begin
    if Assigned(FOnGetThread) then FOnGetThread(Self, ClientSocket, Result);
    if Result = nil then Result := DoCreateThread(ClientSocket);
  end;
end;

//获取负责处理ClientSocket的线程
function TServerWinSocket.GetClientThread(ClientSocket: TServerClientWinSocket): TServerClientThread;
var
  I: Integer;
begin
  Result := nil;
  FListLock.Enter;
  try
    for I := 0 to FActiveThreads.Count - 1 do
      if TServerClientThread(FActiveThreads[I]).ClientSocket = ClientSocket then
      begin
        Result := FActiveThreads[I];
        Break;
      end;
  finally
    FListLock.Leave;
  end;
end;

//服务端收到客户端连接，回调FOnClientConnect
procedure TServerWinSocket.ClientConnect(Socket: TCustomWinSocket);
begin
  if Assigned(FOnClientConnect) then FOnClientConnect(Self, Socket);
end;

//客户端断开连接，回调FOnClientDisconnect
procedure TServerWinSocket.ClientDisconnect(Socket: TCustomWinSocket);
begin
  if Assigned(FOnClientDisconnect) then FOnClientDisconnect(Self, Socket);
  if ServerType = stNonBlocking then Socket.DeferFree;
end;

procedure TServerWinSocket.ClientRead(Socket: TCustomWinSocket);
begin
  if Assigned(FOnClientRead) then FOnClientRead(Self, Socket);
end;

procedure TServerWinSocket.ClientWrite(Socket: TCustomWinSocket);
begin
  if Assigned(FOnClientWrite) then FOnClientWrite(Self, Socket);
end;

procedure TServerWinSocket.ClientErrorEvent(Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  if Assigned(FOnClientError) then FOnClientError(Self, Socket, ErrorEvent, ErrorCode);
end;

{ TServerAcceptThread }
//专门在服务端接受客户端连接的线程
constructor TServerAcceptThread.Create(CreateSuspended: Boolean; ASocket: TServerWinSocket);
begin
  FServerSocket := ASocket;
  inherited Create(CreateSuspended);
end;

procedure TServerAcceptThread.Execute;
begin
  //FServerSocket.Accept是在服务端接收客户端连接
  //所以这个线程持续循环以接收客户端的连接
  while not Terminated do
    FServerSocket.Accept(FServerSocket.SocketHandle);
end;

{ TServerClientThread }
//专门在服务端处理对应客户端连接的函数
constructor TServerClientThread.Create(CreateSuspended: Boolean; ASocket: TServerClientWinSocket);
begin
  FreeOnTerminate := True;
  FEvent := TSimpleEvent.Create;
  inherited Create(True);
  Priority := tpHigher;
  ReActivate(ASocket);
  if not CreateSuspended then Resume;
end;

destructor TServerClientThread.Destroy;
begin
  FClientSocket.Free;
  FEvent.Free;
  inherited Destroy;
end;

procedure TServerClientThread.ReActivate(ASocket: TServerClientWinSocket);
begin
  FClientSocket := ASocket;
  if Assigned(FClientSocket) then
  begin
    FServerSocket := FClientSocket.ServerWinSocket;
    FServerSocket.AddThread(Self);
    FClientSocket.OnSocketEvent := HandleEvent;
    FClientSocket.OnErrorEvent := HandleError;
    FEvent.SetEvent;
  end;
end;

procedure TServerClientThread.DoHandleException;
begin
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  if FException is Exception then
  begin
    if Assigned(ApplicationShowException) then
      ApplicationShowException(FException);
  end else
    SysUtils.ShowException(FException, nil);
end;

procedure TServerClientThread.DoRead;
begin
  ClientSocket.ServerWinSocket.Event(ClientSocket, seRead);
end;

procedure TServerClientThread.DoTerminate;
begin
  inherited DoTerminate;
  if Assigned(FServerSocket) then
    FServerSocket.RemoveThread(Self);
end;

procedure TServerClientThread.DoWrite;
begin
  FServerSocket.Event(ClientSocket, seWrite);
end;

procedure TServerClientThread.HandleEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
begin
  Event(SocketEvent);
end;

procedure TServerClientThread.HandleError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  Error(ErrorEvent, ErrorCode);
end;

procedure TServerClientThread.Event(SocketEvent: TSocketEvent);
begin
  FServerSocket.ClientEvent(Self, ClientSocket, SocketEvent);
end;

procedure TServerClientThread.Error(ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  FServerSocket.ClientError(Self, ClientSocket, ErrorEvent, ErrorCode);
end;

procedure TServerClientThread.HandleException;
begin
  FException := Exception(ExceptObject);
  try
    if not (FException is EAbort) then
      Synchronize(DoHandleException);
  finally
    FException := nil;
  end;
end;

procedure TServerClientThread.Execute;
begin
  //线程开始时先回调OnThreadStart()函数
  FServerSocket.ThreadStart(Self);
  try
    try
      while True do
      begin
        if StartConnect then ClientExecute;
        if EndConnect then Break;
      end;
    except
      HandleException;
      KeepInCache := False;
    end;
  finally
    //线程结束时回调OnThreadEnd()函数
    FServerSocket.ThreadEnd(Self);     
  end;
end;

//主要的线程循环以处理对应的客户端连接
procedure TServerClientThread.ClientExecute;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
begin
  while not Terminated and ClientSocket.Connected do
  begin
    //清空FDSet集合
    FD_ZERO(FDSet);
    //将该客户端Socket放入FDSet集合中
    FD_SET(ClientSocket.SocketHandle, FDSet);
    TimeVal.tv_sec := 0;
    TimeVal.tv_usec := 500;
    //通过select函数将FDSet设置为监听读事件的Socket集合
    //等待TimeVal后，其中有可读的Socket依然在FDSet集合中，可以处理这个集合中的所有Socket去读数据
    //调用DoRead方法去读
    if (select(0, @FDSet, nil, nil, @TimeVal) > 0) and not Terminated then
      if ClientSocket.ReceiveBuf(FDSet, -1) = 0 then Break
      else Synchronize(DoRead);
    //通过select函数将FDSet设置为监听写事件的Socket集合
    //等待TimeVal后，其中有可写的Socket依然在FDSet集合中，可以处理这个集合中的所有Socket去写数据
    //调用DoWrite方法去写
    if (select(0, nil, @FDSet, nil, @TimeVal) > 0) and not Terminated then
      Synchronize(DoWrite);
  end;
end;

function TServerClientThread.StartConnect: Boolean;
begin
  if FEvent.WaitFor(INFINITE) = wrSignaled then
    FEvent.ResetEvent;
  Result := not Terminated;
end;

function TServerClientThread.EndConnect: Boolean;
begin
  FClientSocket.Free;
  FClientSocket := nil;
  Result := Terminated or not KeepInCache;
end;

{ TAbstractSocket }

//回调对应的事件处理函数
procedure TAbstractSocket.DoEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
begin
  Event(Socket, SocketEvent);
end;

//回调对应的错误处理函数
procedure TAbstractSocket.DoError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  Error(Socket, ErrorEvent, ErrorCode);
end;

procedure TAbstractSocket.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      FActive := Value;
    if not (csLoading in ComponentState) then
      DoActivate(Value);
  end;
end;

//初始化Socket
procedure TAbstractSocket.InitSocket(Socket: TCustomWinSocket);
begin
  Socket.OnSocketEvent := DoEvent;
  Socket.OnErrorEvent := DoError;
end;

procedure TAbstractSocket.Loaded;
begin
  inherited Loaded;
  DoActivate(FActive);
end;

//设置IP地址
procedure TAbstractSocket.SetAddress(Value: string);
begin
  if CompareText(Value, FAddress) <> 0 then
  begin
    if not (csLoading in ComponentState) and FActive then
      raise ESocketError.CreateRes(@sCantChangeWhileActive);
    FAddress := Value;
  end;
end;

//设置Host名称
procedure TAbstractSocket.SetHost(Value: string);
begin
  if CompareText(Value, FHost) <> 0 then
  begin
    if not (csLoading in ComponentState) and FActive then
      raise ESocketError.CreateRes(@sCantChangeWhileActive);
    FHost := Value;
  end;
end;

//设置端口
procedure TAbstractSocket.SetPort(Value: Integer);
begin
  if FPort <> Value then
  begin
    if not (csLoading in ComponentState) and FActive then
      raise ESocketError.CreateRes(@sCantChangeWhileActive);
    FPort := Value;
  end;
end;

//？？
procedure TAbstractSocket.SetService(Value: string);
begin
  if CompareText(Value, FService) <> 0 then
  begin
    if not (csLoading in ComponentState) and FActive then
      raise ESocketError.CreateRes(@sCantChangeWhileActive);
    FService := Value;
  end;
end;

//启动
procedure TAbstractSocket.Open;
begin
  Active := True;
end;

//关闭
procedure TAbstractSocket.Close;
begin
  Active := False;
end;

{ TCustomSocket }
//根据不同的事件，去回调开发者对应实现和注册的回调函数
procedure TCustomSocket.Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
begin
  case SocketEvent of
    seLookup: if Assigned(FOnLookup) then FOnLookup(Self, Socket);
    seConnecting: if Assigned(FOnConnecting) then FOnConnecting(Self, Socket);
    seConnect:
      begin
        FActive := True;
        if Assigned(FOnConnect) then FOnConnect(Self, Socket);
      end;
    seListen:
      begin
        FActive := True;
        if Assigned(FOnListen) then FOnListen(Self, Socket);
      end;
    seDisconnect:
      begin
        FActive := False;
        if Assigned(FOnDisconnect) then FOnDisconnect(Self, Socket);
      end;
    seAccept: if Assigned(FOnAccept) then FOnAccept(Self, Socket);
    seRead: if Assigned(FOnRead) then FOnRead(Self, Socket);
    seWrite: if Assigned(FOnWrite) then FOnWrite(Self, Socket);
  end;
end;

//根据不同的错误，去回调开发者对应实现和注册的回调函数
procedure TCustomSocket.Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  if Assigned(FOnError) then FOnError(Self, Socket, ErrorEvent, ErrorCode);
end;

{ TWinSocketStream }
//Socket网络流
constructor TWinSocketStream.Create(ASocket: TCustomWinSocket; TimeOut: Longint);
begin
  if ASocket.ASyncStyles <> [] then
    raise ESocketError.CreateRes(@sSocketMustBeBlocking);
  FSocket := ASocket;
  FTimeOut := TimeOut;
  FEvent := TSimpleEvent.Create;
  inherited Create;
end;

destructor TWinSocketStream.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

//等待可读
function TWinSocketStream.WaitForData(Timeout: Longint): Boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
begin
  TimeVal.tv_sec := Timeout div 1000;
  TimeVal.tv_usec := (Timeout mod 1000) * 1000;
  FD_ZERO(FDSet);
  FD_SET(FSocket.SocketHandle, FDSet);
  Result := select(0, @FDSet, nil, nil, @TimeVal) > 0;
end;

//从Buffer开始读取Count字节的数据
function TWinSocketStream.Read(var Buffer; Count: Longint): Longint;
var
  Overlapped: TOverlapped;
  ErrorCode: Integer;
begin
  FSocket.Lock;
  try
    FillChar(OVerlapped, SizeOf(Overlapped), 0);
    Overlapped.hEvent := FEvent.Handle;
    if not ReadFile(FSocket.SocketHandle, Buffer, Count, DWORD(Result),
      @Overlapped) and (GetLastError <> ERROR_IO_PENDING) then
    begin
      ErrorCode := GetLastError;
      raise ESocketError.CreateResFmt(@sSocketIOError, [sSocketRead, ErrorCode,
        SysErrorMessage(ErrorCode)]);
    end;
    if FEvent.WaitFor(FTimeOut) <> wrSignaled then
      Result := 0
    else
    begin
      GetOverlappedResult(FSocket.SocketHandle, Overlapped, DWORD(Result), False);
      FEvent.ResetEvent;
    end;
  finally
    FSocket.Unlock;
  end;
end;

//往Buffer中写入Count字节的数据
function TWinSocketStream.Write(const Buffer; Count: Longint): Longint;
var
  Overlapped: TOverlapped;
  ErrorCode: Integer;
begin
  FSocket.Lock;
  try
    FillChar(OVerlapped, SizeOf(Overlapped), 0);
    Overlapped.hEvent := FEvent.Handle;
    if not WriteFile(FSocket.SocketHandle, Buffer, Count, DWORD(Result),
      @Overlapped) and (GetLastError <> ERROR_IO_PENDING) then
    begin
      ErrorCode := GetLastError;
      raise ESocketError.CreateResFmt(@sSocketIOError, [sSocketWrite, ErrorCode,
        SysErrorMessage(ErrorCode)]);
    end;
    if FEvent.WaitFor(FTimeOut) <> wrSignaled then
      Result := 0
    else GetOverlappedResult(FSocket.SocketHandle, Overlapped, DWORD(Result), False);
  finally
    FSocket.Unlock;
  end;
end;

function TWinSocketStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;

{ TClientSocket }
//客户端Socket编程组件
constructor TClientSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientSocket := TClientWinSocket.Create(INVALID_SOCKET);      //创建一个TClientWinSocket对象，用于进行通信
  InitSocket(FClientSocket);                                     //初始化TClientWinSocket对象
end;

destructor TClientSocket.Destroy;
begin
  FClientSocket.Free;
  inherited Destroy;
end;

procedure TClientSocket.DoActivate(Value: Boolean);
begin
  if (Value <> FClientSocket.Connected) and not (csDesigning in ComponentState) then
  begin
    if FClientSocket.Connected then
      FClientSocket.Disconnect(FClientSocket.FSocket)
    else FClientSocket.Open(FHost, FAddress, FService, FPort, ClientType = ctBlocking);
  end;
end;

//获取客户端类型：阻塞 or 非阻塞
function TClientSocket.GetClientType: TClientType;
begin
  Result := FClientSocket.ClientType;
end;

//设置客户端类型
procedure TClientSocket.SetClientType(Value: TClientType);
begin
  FClientSocket.ClientType := Value;
end;

{ TCustomServerSocket }

destructor TCustomServerSocket.Destroy;
begin
  FServerSocket.Free;
  inherited Destroy;
end;

procedure TCustomServerSocket.DoActivate(Value: Boolean);
begin
  if (Value <> FServerSocket.Connected) and not (csDesigning in ComponentState) then
  begin
    if FServerSocket.Connected then
      FServerSocket.Disconnect(FServerSocket.SocketHandle)
    else FServerSocket.Listen(FHost, FAddress, FService, FPort, SOMAXCONN);
  end;
end;

function TCustomServerSocket.GetServerType: TServerType;
begin
  Result := FServerSocket.ServerType;
end;

procedure TCustomServerSocket.SetServerType(Value: TServerType);
begin
  FServerSocket.ServerType := Value;
end;

function TCustomServerSocket.GetGetThreadEvent: TGetThreadEvent;
begin
  Result := FServerSocket.OnGetThread;
end;

procedure TCustomServerSocket.SetGetThreadEvent(Value: TGetThreadEvent);
begin
  FServerSocket.OnGetThread := Value;
end;

function TCustomServerSocket.GetGetSocketEvent: TGetSocketEvent;
begin
  Result := FServerSocket.OnGetSocket;
end;

procedure TCustomServerSocket.SetGetSocketEvent(Value: TGetSocketEvent);
begin
  FServerSocket.OnGetSocket := Value;
end;

function TCustomServerSocket.GetThreadCacheSize: Integer;
begin
  Result := FServerSocket.ThreadCacheSize;
end;

procedure TCustomServerSocket.SetThreadCacheSize(Value: Integer);
begin
  FServerSocket.ThreadCacheSize := Value;
end;

function TCustomServerSocket.GetOnThreadStart: TThreadNotifyEvent;
begin
  Result := FServerSocket.OnThreadStart;
end;

function TCustomServerSocket.GetOnThreadEnd: TThreadNotifyEvent;
begin
  Result := FServerSocket.OnThreadEnd;
end;

procedure TCustomServerSocket.SetOnThreadStart(Value: TThreadNotifyEvent);
begin
  FServerSocket.OnThreadStart := Value;
end;

procedure TCustomServerSocket.SetOnThreadEnd(Value: TThreadNotifyEvent);
begin
  FServerSocket.OnThreadEnd := Value;
end;

function TCustomServerSocket.GetOnClientEvent(Index: Integer): TSocketNotifyEvent;
begin
  case Index of
    0: Result := FServerSocket.OnClientRead;
    1: Result := FServerSocket.OnClientWrite;
    2: Result := FServerSocket.OnClientConnect;
    3: Result := FServerSocket.OnClientDisconnect;
  end;
end;

procedure TCustomServerSocket.SetOnClientEvent(Index: Integer; Value: TSocketNotifyEvent);
begin
  case Index of
    0: FServerSocket.OnClientRead := Value;
    1: FServerSocket.OnClientWrite := Value;
    2: FServerSocket.OnClientConnect := Value;
    3: FServerSocket.OnClientDisconnect := Value;
  end;
end;

function TCustomServerSocket.GetOnClientError: TSocketErrorEvent;
begin
  Result := FServerSocket.OnClientError;
end;

procedure TCustomServerSocket.SetOnClientError(Value: TSocketErrorEvent);
begin
  FServerSocket.OnClientError := Value;
end;

{ TServerSocket }
//服务端Socket编程组件
constructor TServerSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerSocket := TServerWinSocket.Create(INVALID_SOCKET);
  InitSocket(FServerSocket);
  FServerSocket.ThreadCacheSize := 10;
end;

end.
