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
  //���ݱ��������룺Socket��Ϣ
  CM_SOCKETMESSAGE = WM_USER + $0001;
  CM_DEFERFREE = WM_USER + $0002;
  CM_LOOKUPCOMPLETE = WM_USER + $0003;

type
  //����Socketͨ���е��쳣�࣬�̳���Exception
  ESocketError = class(Exception);

  //Socket��Ϣ�ṹ��
  TCMSocketMessage = record
    Msg: Cardinal;
    Socket: TSocket;
    SelectEvent: Word;
    SelectError: Word;
    Result: Longint;
  end;

  //LookupComplete �����Ǹ�ʲô������
  TCMLookupComplete = record
    Msg: Cardinal;
    LookupHandle: THandle;
    AsyncBufLen: Word;
    AsyncError: Word;
    Result: Longint;
  end;

  //ScktComp�ж���ļ�����
  TCustomWinSocket = class;             //����ˡ��ͻ���Socket�ĸ���
  TCustomSocket = class;                //Delphi����õ����࣬��֤ServerSocket��ClientSocket���̳��Դ���
  TServerAcceptThread = class;          //�����Accept�ͻ������ӵ��߳�
  TServerClientThread = class;          //����ģʽ�£�����ÿ���ͻ������ӣ������ר�Ŵ���һ�����߳�ȥ���д���
  TServerWinSocket = class;             //����˼���Socket
  TServerClientWinSocket = class;       //����˱�ʾ�ͻ������ӵ�Socket

  //�����IO ���ͣ�������������
  TServerType = (stNonBlocking, stThreadBlocking);
  //�ͻ���IO ���ͣ�������������
  TClientType = (ctNonBlocking, ctBlocking);
  //�첽��ʽ�����첽��д�첽�������쳣�����������첽�������첽���ر������첽
  TAsyncStyle = (asRead, asWrite, asOOB, asAccept, asConnect, asClose);
  //�첽��ʽ���ϣ���Ϊͬʱ����ѡ������첽��ʽ
  TAsyncStyles = set of TAsyncStyle;
  //Socket�¼���Lookup�����������ӡ����ӡ��Ͽ����ӡ��������������ӡ��������ݡ���������
  TSocketEvent = (seLookup, seConnecting, seConnect, seDisconnect, seListen, seAccept, seWrite, seRead);
  //Lookup״̬��ʲô��lookup
  TLookupState = (lsIdle, lsLookupAddress, lsLookupService);
  //�����¼���ͨ�ô��󡢷��ʹ��󡢽��մ������Ӵ��󡢶Ͽ����Ӵ��󡢽������Ӵ���Lookup����
  TErrorEvent = (eeGeneral, eeSend, eeReceive, eeConnect, eeDisconnect, eeAccept, eeLookup);

  //2017-03-30�����⣺ʲô��Lookup

  //����Socket�¼�����ָ������
  TSocketEventEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent) of object;
  //����Socket�����¼�����ָ������
  TSocketErrorEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer) of object;
  //����
  TGetSocketEvent = procedure (Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket) of object;
  //����
  TGetThreadEvent = procedure (Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread) of object;
  //����
  TSocketNotifyEvent = procedure (Sender: TObject; Socket: TCustomWinSocket) of object;

  TCustomWinSocket = class
  private
    FSocket: TSocket;                   //TSocket = u_int;
    FConnected: Boolean;                //�Ƿ����ӳɹ�
    FSendStream: TStream;               //������
    FDropAfterSend: Boolean;
    FHandle: HWnd;
    FAddr: TSockAddrIn;                 //��ַ
    FAsyncStyles: TASyncStyles;         //TAsyncStyles = set of TAsyncStyle;
    FLookupState: TLookupState;         //TLookupState = (lsIdle, lsLookupAddress, lsLookupService);
    FLookupHandle: THandle;             //
    FOnSocketEvent: TSocketEventEvent;  //TSocketEventEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent) of object;
    FOnErrorEvent: TSocketErrorEvent;   //TSocketErrorEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer) of object;
    FSocketLock: TCriticalSection;      //ʹ���ٽ�����֤�̰߳�ȫ
    FGetHostData: Pointer;
    FData: Pointer;
    // Used during non-blocking host and service lookups
    FService: string;                   //
    FPort: Word;                        //�˿�
    FClient: Boolean;                   //��ʾ��Socket�ǲ��ǿͻ���
    FQueueSize: Integer;                //�������Ӷ��д�С
    function SendStreamPiece: Boolean;  
    procedure WndProc(var Message: TMessage);                                               //�ڸú����ڲ��ַ���Ϣ
    procedure CMLookupComplete(var Message: TCMLookupComplete); message CM_LOOKUPCOMPLETE;  //�ú���������ӦCM_LOOKUPCOMPLETE��Ϣ
    procedure CMSocketMessage(var Message: TCMSocketMessage); message CM_SOCKETMESSAGE;     //�ú���������ӦCM_SOCKETMESSAGE��Ϣ
    procedure CMDeferFree(var Message); message CM_DEFERFREE;                               //�ú���������ӦCM_DEFERFREE��Ϣ
    procedure DeferFree;                //����
    procedure DoSetAsyncStyles;         //����
    function GetHandle: HWnd;           //����
    function GetLocalHost: string;      //��ȡ������Host����
    function GetLocalAddress: string;   //��ȡ������IP��ַ
    function GetLocalPort: Integer;     //��ȡ����Socket�Ķ˿ں�
    function GetRemoteHost: string;     //��ȡ�Է�������Host����
    function GetRemoteAddress: string;  //��ȡ�Է�������IP��ַ
    function GetRemotePort: Integer;    //��ȡ�Է�����Socket�Ķ˿ں�
    function GetRemoteAddr: TSockAddrIn;//��ȡ�Է������ĵ�ַ��IP���˿ڣ���Ϣ
  protected
    //���첽�ķ�ʽ��ʼ��Socket
    procedure AsyncInitSocket(const Name, Address, Service: string; Port: Word; QueueSize: Integer; Client: Boolean);
    //�������ӣ�
    procedure DoOpen;
    //������
    procedure DoListen(QueueSize: Integer);
    //��ʼ��Socket
    function InitSocket(const Name, Address, Service: string; Port: Word; Client: Boolean): TSockAddrIn;
    //�ص���Ӧ��SocketEvent�¼�������
    procedure Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent); dynamic;
    //�ص���Ӧ��ErrorEvent��������
    procedure Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); dynamic;
    //�����첽��ʽ
    procedure SetAsyncStyles(Value: TASyncStyles);
  public
    constructor Create(ASocket: TSocket);
    destructor Destroy; override;
    procedure Close;
    procedure DefaultHandler(var Message); override;
    procedure Lock;      //����
    procedure Unlock;    //����
    //����˼����˿�
    procedure Listen(const Name, Address, Service: string; Port: Word; QueueSize: Integer; Block: Boolean = True);
    //��Socket
    procedure Open(const Name, Address, Service: string; Port: Word; Block: Boolean = True);
    //����˽��տͻ�������
    procedure Accept(Socket: TSocket); virtual;
    //��������
    procedure Connect(Socket: TSocket); virtual;
    //�Ͽ�����
    procedure Disconnect(Socket: TSocket); virtual;
    //��TCP�������ж�����
    procedure Read(Socket: TSocket); virtual;
    //��TCP��������д����
    procedure Write(Socket: TSocket); virtual;
    //Lookup������ɶ��
    function LookupName(const name: string): TInAddr;
    function LookupService(const service: string): Integer;

    //��ȡ��ǰTCP�����������������ο����£�http://www.xumenger.com/scktcomp-test-20170329/
    function ReceiveLength: Integer;
    //��TCP�������ж�ȡ���ݣ��ο����£�http://www.xumenger.com/delphi-binary-socket-20161222/
    function ReceiveBuf(var Buf; Count: Integer): Integer;
    //���ı���ʽ��TCP�������ж�ȡ���ݣ����������Ƕ��������ݣ����ַ����Ͳ�����
    function ReceiveText: string;
    //��������
    function SendBuf(var Buf; Count: Integer): Integer;
    //������������������ļ������Ϳ���ʵ�ֽ��ļ��е����ݷ��ͳ�ȥ
    function SendStream(AStream: TStream): Boolean;
    //����
    function SendStreamThenDrop(AStream: TStream): Boolean;
    //�����ı�
    function SendText(const S: string): Integer;

    property LocalHost: string read GetLocalHost;        //��ȡ����Host��
    property LocalAddress: string read GetLocalAddress;  //��ȡ����IP��ַ
    property LocalPort: Integer read GetLocalPort;       //��ȡ�����˿�

    property RemoteHost: string read GetRemoteHost;      //��ȡ�Է�Host��
    property RemoteAddress: string read GetRemoteAddress;//��ȡ�Է�IP
    property RemotePort: Integer read GetRemotePort;     //��ȡ�Է��˿�
    property RemoteAddr: TSockAddrIn read GetRemoteAddr; //��ȡ�Է���ַ��Ϣ

    property Connected: Boolean read FConnected;         //�Ƿ����ӳɹ�
    property Addr: TSockAddrIn read FAddr;
    property ASyncStyles: TAsyncStyles read FAsyncStyles write SetAsyncStyles;  //�첽��ʽ
    property Handle: HWnd read GetHandle;
    property SocketHandle: TSocket read FSocket;
    property LookupState: TLookupState read FLookupState;//Lookup������ɶ����

    //Socket�¼��ص�
    property OnSocketEvent: TSocketEventEvent read FOnSocketEvent write FOnSocketEvent;
    //Socket����ص�
    property OnErrorEvent: TSocketErrorEvent read FOnErrorEvent write FOnErrorEvent;

    property Data: Pointer read FData write FData;
  end;

  //TClientWinSocket�̳���TCustomWinSocket��ʵ�ֿͻ��˶��е�һЩ����
  //TCustomWinSocket�����װ�˿ͻ��˺ͷ������ͬ���߼�
  //Ȼ��TClientWinSocketʵ�ֿͻ������е��߼�
  //������ڱ������Ҫ�ĳ��󣬽���ͬ�Ĳ��ֳ����������ͬ�Ĳ���ͨ���̳еķ�ʽ�ɶ�Ӧ������ʵ��
  TClientWinSocket = class(TCustomWinSocket)
  private
    FClientType: TClientType;                    //�ͻ������ͣ�TClientType = (ctNonBlocking, ctBlocking);
  protected
    procedure SetClientType(Value: TClientType); //���ÿͻ�������
  public
    procedure Connect(Socket: TSocket); override;//ֻ�пͻ����з������ӵ��߼������Կͻ�����Ҫʵ�����ӷ���
    property ClientType: TClientType read FClientType write SetClientType;
  end;

  //TServerClientWinSocket�̳���TCustomWinSocket
  //���������ڷ���˱�ʾһ���ͻ������Ӷ���
  TServerClientWinSocket = class(TCustomWinSocket)
  private
    FServerWinSocket: TServerWinSocket;
  public
    constructor Create(Socket: TSocket; ServerWinSocket: TServerWinSocket);
    destructor Destroy; override;

    property ServerWinSocket: TServerWinSocket read FServerWinSocket;
  end;

  TThreadNotifyEvent = procedure (Sender: TObject; Thread: TServerClientThread) of object;

  //TServerWinSocket�̳���TCustomWinSocket��ʵ�ַ����һЩ���еĹ���
  //����������Listen��Accept���ܣ����ͻ��˲�û��
  TServerWinSocket = class(TCustomWinSocket)
  private
    FServerType: TServerType;                //��������ͣ�TServerType = (stNonBlocking, stThreadBlocking);
    FThreadCacheSize: Integer;               //�̹߳���List���߳�����
    FConnections: TList;                     //���ڴ���Ѿ����ӵĿͻ���Socket����ŵ������ǣ���AClient: TServerClientWinSocket��
    FActiveThreads: TList;                   //��ŵ��߳���AThread: TServerClientThread �����������Ϊ����������£�ÿ���ͻ������Ӷ�Ӧ����һ��TServerClientThread�߳�ȥ����ÿͻ��˵�����
    FListLock: TCriticalSection;             //������������֤�̰߳�ȫ
    FServerAcceptThread: TServerAcceptThread;//���߳�����ѭ�����տͻ��˵�����
    FOnGetSocket: TGetSocketEvent;           //TGetSocketEvent = procedure (Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket) of object;
    FOnGetThread: TGetThreadEvent;           //TGetThreadEvent = procedure (Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread) of object;

    //TThreadNotifyEvent = procedure (Sender: TObject; Thread: TServerClientThread) of object;
    FOnThreadStart: TThreadNotifyEvent;      //�ûص�������������һ��TServerClientThread�̣߳���ThreadStart�����лص��ú���ָ��
    FOnThreadEnd: TThreadNotifyEvent;        //�ûص��������ڽ���һ��TServerClientThread�̣߳���ThreadEnd�����лص��ú���ָ��

    //TSocketNotifyEvent = procedure (Sender: TObject; Socket: TCustomWinSocket) of object;
    FOnClientConnect: TSocketNotifyEvent;    //�յ��ͻ������Ӻ���¼�
    FOnClientDisconnect: TSocketNotifyEvent; //�ͻ��˶Ͽ����ӵ��¼�
    FOnClientRead: TSocketNotifyEvent;
    FOnClientWrite: TSocketNotifyEvent;      //

    //TSocketErrorEvent = procedure (Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer) of object;
    FOnClientError: TSocketErrorEvent;       //�ͻ������ӷ���������¼�

    procedure AddClient(AClient: TServerClientWinSocket);         //��FConnections: TList �����һ�����ӳɹ��Ŀͻ���Socket
    procedure RemoveClient(AClient: TServerClientWinSocket);      //��FConnections: TList �Ƴ�һ�����ӳɹ��Ŀͻ���Socket
    procedure AddThread(AThread: TServerClientThread);            //��FActiveThreads �����һ�� TServerClientThread�߳�
    procedure RemoveThread(AThread: TServerClientThread);         //��FActiveThreads ���Ƴ�һ�� TServerClientThread�߳�
    //���ݲ�ͬ��SocketEvent���ͣ��ص���ͬ���¼��ص�����ָ�룬���崦����ЩSocketEvent����ϸ�μ���Ӧʵ�ִ���
    procedure ClientEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
    //���ݲ�ͬ��ErrorEvent���ͣ��ص���ͬ�Ĵ���ص�����ָ��
    procedure ClientError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    function GetActiveConnections: Integer;                            //��ȡ��Ȼ���ŵĿͻ������Ӹ�����Ҳ���Ƿ��� FConnections.Count
    function GetActiveThreads: Integer;                                //��ȡ��Ȼ���ŵ��̸߳�����Ҳ���Ƿ��� FActiveThreads.Count
    function GetConnections(Index: Integer): TCustomWinSocket;         //��FConnections�л�ȡ��Index���ͻ�������Socket
    function GetIdleThreads: Integer;
  protected
    //Ϊÿ���ͻ���TServerClientWinSocket������һ��TServerClientThread�߳�
    function DoCreateThread(ClientSocket: TServerClientWinSocket): TServerClientThread; virtual;
    procedure Listen(var Name, Address, Service: string; Port: Word; QueueSize: Integer);   //��������˶˿�
    procedure SetServerType(Value: TServerType);                      //���÷���˵����ͣ����������Ƿ�����
    procedure SetThreadCacheSize(Value: Integer);                     //�����̳߳ظ���
    procedure ThreadEnd(AThread: TServerClientThread); dynamic;       //����һ��TServerClientThread�߳�
    procedure ThreadStart(AThread: TServerClientThread); dynamic;     //����һ��TServerClientThread
    function GetClientSocket(Socket: TSocket): TServerClientWinSocket; dynamic;     //����ԭ����TSocket������һ��TServerClientWinSocket���ڷ����ÿ���յ�һ���ͻ�������ʱ�õõ�
    function GetServerThread(ClientSocket: TServerClientWinSocket): TServerClientThread; dynamic;   //ΪTServerClientWinSocket����һ��ר�ŵ�TServerClientThread�߳����������TServerClientWinSocket���������
    procedure ClientRead(Socket: TCustomWinSocket); dynamic;          //���տͻ��˷��͵����ݣ����лص�FOnClientRead(Self, Socket);
    procedure ClientWrite(Socket: TCustomWinSOcket); dynamic;         //���ͻ��˷������ݣ����лص�FOnClientConnect(Self, Socket);
    procedure ClientConnect(Socket: TCustomWinSOcket); dynamic;       //�յ��ͻ������ӣ����лص�FOnClientConnect(Self, Socket);
    procedure ClientDisconnect(Socket: TCustomWinSOcket); dynamic;    //�ͻ��˶Ͽ����ӣ����лص�FOnClientDisconnect(Self, Socket);
    //�ͻ������ӳ������лص�FOnClientError(Self, Socket, ErrorEvent, ErrorCode);
    procedure ClientErrorEvent(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); dynamic;
  public
    constructor Create(ASocket: TSocket);
    destructor Destroy; override;
    procedure Accept(Socket: TSocket); override;        //���տͻ�������
    procedure Disconnect(Socket: TSocket); override;    //�Ϳͻ��˶Ͽ�����
    function GetClientThread(ClientSocket: TServerClientWinSocket): TServerClientThread;   //��ȡTServerClientWinSocket��Ӧ�Ĵ����߳�
    property ActiveConnections: Integer read GetActiveConnections;                         //��ȡ��ǰ���ŵĿͻ������Ӹ���
    property ActiveThreads: Integer read GetActiveThreads;                                 //��ȡ��ǰ���ߵĿͻ��˴����̸߳���
    property Connections[Index: Integer]: TCustomWinSocket read GetConnections;            //��ȡ��Index���ͻ�������
    property IdleThreads: Integer read GetIdleThreads;                                     //
    property ServerType: TServerType read FServerType write SetServerType;                 //��ȡ��������ͣ�������������
    property ThreadCacheSize: Integer read FThreadCacheSize write SetThreadCacheSize;      //��
    property OnGetSocket: TGetSocketEvent read FOnGetSocket write FOnGetSocket;            //��
    property OnGetThread: TGetThreadEvent read FOnGetThread write FOnGetThread;            //��
    property OnThreadStart: TThreadNotifyEvent read FOnThreadStart write FOnThreadStart;   //�����̻߳ص�
    property OnThreadEnd: TThreadNotifyEvent read FOnThreadEnd write FOnThreadEnd;         //�����̻߳ص�
    property OnClientConnect: TSocketNotifyEvent read FOnClientConnect write FOnClientConnect;           //�յ��ͻ������ӻص�
    property OnClientDisconnect: TSocketNotifyEvent read FOnClientDisconnect write FOnClientDisconnect;  //�ͻ��˶Ͽ����ӻص�
    property OnClientRead: TSocketNotifyEvent read FOnClientRead write FOnClientRead;                    //��
    property OnClientWrite: TSocketNotifyEvent read FOnClientWrite write FOnClientWrite;                 //��
    property OnClientError: TSocketErrorEvent read FOnClientError write FOnClientError;                  //��
  end;

  //���߳�ר������ѭ�����տͻ�������
  TServerAcceptThread = class(TThread)
  private
    FServerSocket: TServerWinSocket;
  public
    constructor Create(CreateSuspended: Boolean; ASocket: TServerWinSocket);
    procedure Execute; override;

    property ServerSocket: TServerWinSocket read FServerSocket;
  end;

  //���߳��ڷ����Ϊ����ģʽ�����ã�����ģʽ�£�����˻����ÿ���ͻ������ӵ�������һ�����߳�
  TServerClientThread = class(TThread)
  private
    FClientSocket: TServerClientWinSocket;     //��Ӧ�Ŀͻ�������Socket
    FServerSocket: TServerWinSocket;           //�������ķ����Socket
    FException: Exception;                     //�쳣
    {TEvent����SyncObjs��Ԫ�ж�����࣬��ʵTHandleObject������࣬Ҫʹ������Ҫ��uses SyncObjs
    TEvent���ڶ��̻߳����п������������߳�ͬ�������ڵ��̻߳����п����ڵ�����Ӧ��ͬ���첽�¼�����ϵͳ��Ϣ���û��������Ĵ����
    TEventͬ������ʹ��WaitFor()��WaitForMultiple()����
    ��Ҫע����ǣ�TEvent�ಢû��ʵ��Acquire����
    Delphi�ж�����һ�����򵥵��¼��࣬TSimpleEvent�࣬����Դ���Ͽ����������TSimpleEvent=class(TEvent);һ�䣬��û�����κ�����TSimpleEvent�ĳ�Ա����������Ϊ�����ݶ�����
      TEvent.SetEvent()   ��Ϊ���ź�״̬
      TEvent.ReSetEvent() ��Ϊ�����ź�״̬
      TEvent.WaitFor()    �ȴ���ֱ��������SetEvent()������Ϊ���ź�״̬
      TEventʹ�õ������ź�����FEvent: TSemaphore;}
    FEvent: TSimpleEvent;                      //
    FKeepInCache: Boolean;                     //����
    FData: Pointer;                            //����
    procedure HandleEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
    procedure HandleError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure DoHandleException;
    procedure DoRead;
    procedure DoWrite;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure ClientExecute; virtual;         //ѭ��ʹ��selectģ��ȥ����д�ͻ�������Socket
    procedure Event(SocketEvent: TSocketEvent); virtual;                        //�ص���ӦTServerWinSocket��ClientEvent����
    procedure Error(ErrorEvent: TErrorEvent; var ErrorCode: Integer); virtual;  //�ص���ӦTServerWinSocket��ClientError����
    procedure HandleException; virtual;
    //������߳����»�������������Ϊÿ���ͻ��˴���һ���̵߳�ʱ������ÿͻ��˶Ͽ������ˣ���ô����߳̾͡����ˡ������ǲ��ͷŸ��߳�
    //�ȵ���һ���ͻ������ӹ�����ʱ�򣬾ʹӷ���˵��߳�������ȡ��һ�������ˡ����̣߳��������������µĿͻ�������
    //��˾Ϳ��Բ���Ƶ���Ĵ����������̣߳���ʡ�˺ܶ಻��Ҫ����Դ
    procedure ReActivate(ASocket: TServerClientWinSocket);
    function StartConnect: Boolean;                //����
    function EndConnect: Boolean;                  //����
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
    FActive: Boolean;           //�����жϿͻ���/�������Ƿ�����
    FPort: Integer;             //�˿ں�
    FAddress: string;           //IP ��ַ
    FHost: string;              //Host ����
    FService: string;
    //�ص���Event(Socket, SocketEvent);
    procedure DoEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
     //�ص���Error(Socket, ErrorEvent, ErrorCode);
    procedure DoError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  protected
    //���󷽷�������Socket�¼�����Ҫ����ʵ��
    procedure Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent); virtual; abstract;
    //���󷽷�������Socket������Ҫ����ʵ��
    procedure Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); virtual; abstract;
    procedure DoActivate(Value: Boolean); virtual; abstract;
    procedure InitSocket(Socket: TCustomWinSocket);         //��ʼ��Socket
    procedure Loaded; override;                             //����
    procedure SetActive(Value: Boolean);                    //
    procedure SetAddress(Value: string);                    //����IP��ַ
    procedure SetHost(Value: string);                       //����Host����
    procedure SetPort(Value: Integer);                      //���ö˿�
    procedure SetService(Value: string);                    //����
    property Active: Boolean read FActive write SetActive;
    property Address: string read FAddress write SetAddress;
    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Service: string read FService write SetService;
  public
    procedure Open;                                         //Active := True;
    procedure Close;                                        //Active := False;
  end;

  //TCustomSocket�̳���TAbstractSocket
  //֮ǰ˵�����ӡ������ݵ��¼������д���Ļص������������ǿ�����������ͨ��ָ��FOnLookup��FOnConnect�����õ�
  //�ȵ���Ӧ���¼�/������ֵ�ʱ���ڸ����Event��Error�����и����¼�/��������ͻص�������ע��Ķ�Ӧ����
  TCustomSocket = class(TAbstractSocket)
  private
    FOnLookup: TSocketNotifyEvent;             //����
    FOnConnect: TSocketNotifyEvent;            //�����¼�
    FOnConnecting: TSocketNotifyEvent;         //���������¼�
    FOnDisconnect: TSocketNotifyEvent;         //�Ͽ������¼�
    FOnListen: TSocketNotifyEvent;             //�����˿��¼�
    FOnAccept: TSocketNotifyEvent;             //���տͻ��������¼�
    FOnRead: TSocketNotifyEvent;               //�������������¼�
    FOnWrite: TSocketNotifyEvent;              //�������������¼�
    FOnError: TSocketErrorEvent;               //��������
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

  //������
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

  //�ͻ��˱���������ʹ��ScktComp���пͻ��˱��ʱ��������������
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
  //ͨ��publised�ؼ��ֽ���Щ�����ڿ����������ʾ
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

//threadvar�ؼ��ֵ������ǣ��ñ�����ÿ���߳�����һ��
threadvar
  SocketErrorProc: TSocketErrorProc;

var
  WSAData: TWSAData;

//����Socket��������
function SetErrorProc(ErrorProc: TSocketErrorProc): TSocketErrorProc;
begin
  Result := SocketErrorProc;
  SocketErrorProc := ErrorProc;
end;

//���Socket�Ľ��
function CheckSocketResult(ResultCode: Integer; const Op: string): Integer;
begin
  if ResultCode <> 0 then
  begin
    Result := WSAGetLastError;      //��ȡ���һ�ε�Socket�������
    //WSAEWOULDBLOCK��10035������˼��Output Buffer�Ѿ����ˣ��޷���д������
    //ȷ�е�˵����ʵ�����Ǹ����󣬳��������쳣�ľ��󲿷�ʱ����ʵ��������Output Buffer��������������ǳ���һ�֡�æ����״̬
    //�����֡�æ����״̬���ܴ�̶��������ڽ��շ���ɵ�
    //��˼������Ҫ���͵Ķ��󣬶Է��յ�û���㷢�Ŀ죬��Է��Ľ��ջ������ѱ����������Ծͷ�����һ����æ���ı�־
    //��ʱ�����ٷ��Ͷ������ݶ�û���κ����壬�������ϵͳ���׳��� WSAEWOULDBLOCK�쳣֪ͨ�㣬�����Ϲæ����
    //��ô�����WSAEWOULDBLOCKӦ����ô���أ������кܶ����ѵ��������������������Sleepһ��ʱ�䣬һ�����ͣ�ٺ�Output Buffer�Ϳճ����ˣ��ǾͿ��Լ���������
    //�����Ƽ�����ķ���������MSDN�ĵ���ʾ��������WSAEWOULDBLOCK�쳣��ֱ���ճ�Output Bufferʱ��ϵͳ�ᷢ��һ��FD_WRITE�����ͷ�
    //������ȫ�����ڵȵ�FD_WRITE��Ϣ�������·����쳣��ʼ�����ݰ����ɣ��ð���Ҫȫ�����·��ͣ�
    if Result <> WSAEWOULDBLOCK then
      if Assigned(SocketErrorProc) then
        SocketErrorProc(Result)
      else raise ESocketError.CreateResFmt(@sWindowsSocketError,
        [SysErrorMessage(Result), Result, Op]);
  end else Result := 0;
end;

//Windows�������У��ʼ������ʹ��WSAStartup
procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise ESocketError.CreateResFmt(@sWindowsSocketError,
      [SysErrorMessage(ErrorCode), ErrorCode, 'WSAStartup']);
end;

//��WSAStartup��Ӧ�ģ�����������˳�ʱ��Ҫ����WSACleanup
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
  FASyncStyles := [asRead, asWrite, asConnect, asClose];  //����д�����ӡ��ر�ʱ�첽
  FSocket := ASocket;                                     //����Windowsԭ����Socket
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
  FreeMem(FGetHostData);                                  //���ڽṹ�����ͷ�ʱ��������һ����nil�Ĵ���
  FGetHostData := nil;
  inherited Destroy;
end;

//���տͻ������ӣ���ΪTCustomWinSocket�ᱻ�ͻ���Socket�������Socket������˵Ŀͻ���Socket
//��ֻ�з����Socket�Ż�Accept������������Acceptû���ڸ�����ʵ�֣�������ڷ����Socket��ʵ��
procedure TCustomWinSocket.Accept(Socket: TSocket);
begin
end;

//�첽�ķ�ʽ��ʼ��Socket
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

            //WSAAsyncGetHostByName������gethostbyname()���첽�汾����������ȡ��Ӧ��һ�����������������ƺ͵�ַ��Ϣ
            FLookupHandle := WSAAsyncGetHostByName(Handle, CM_LOOKUPCOMPLETE,
              PChar(Name), FGetHostData, MAXGETHOSTSTRUCT);
            CheckSocketResult(Ord(FLookupHandle = 0), 'WSAASyncGetHostByName');
            FService := Service;
            FPort := Port;
            FQueueSize := QueueSize;            //�������д�С 
            FClient := Client;                  //��Socket�ǲ��ǿͻ���Socket
            FLookupState := lsLookupAddress;    //����Lookup������ɶ
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
              
            //WSAASyncGetServByName��getservbyname()���첽�汾����������ȡ��Ӧ��һ���������ķ�����Ϣ
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

    //�������ú����Լ�
    if FLookupState <> lsIdle then
      ASyncInitSocket(Name, Address, Service, Port, QueueSize, Client);
  except
    Disconnect(FSocket);
    raise;
  end;
end;

//�ر�Socket��Ҳ���ǶϿ�����
procedure TCustomWinSocket.Close;
begin
  Disconnect(FSocket);
end;

//�������ӣ���ΪTCustomWinSocket�ᱻ�ͻ���Socket�������Socket������˵Ŀͻ���Socket
//��ֻ�пͻ���Socket�Ż�Connect������������Connectû���ڸ�����ʵ�֣�������ڷ����Socket��ʵ��
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
        FD_CONNECT: Connect(Socket);         //����Connect��������ӦFD_CONNECT
        FD_CLOSE: Disconnect(Socket);
        FD_READ: Read(Socket);
        FD_WRITE: Write(Socket);
        FD_ACCEPT: Accept(Socket);
      end;
{WSAAsyncSelectģ������Ӧ�ó�����Windows��Ϣ�ķ�ʽ���������¼�֪ͨ����������Ҫ�󲻸ߵ�����Ӧ�ó��򶼲���WSAAsyncSelectģ��
WSAAsyncSelect�Զ����׽�������Ϊ������ģʽ������Ϊ�׽��ְ�һ�����ھ�������������¼�����ʱ������������ڷ�����Ϣ
int WSAAsyncSelect(
  SOCKET s,     //��Ҫ���õ��׽��־��
  HWND, hWnd,   //ָ��һ�����ھ�����׽��ֵ�֪ͨ��Ϣ���������˴�����
  u_int wMsg,   //�����¼�������ID��������WM_USER������ֵ������ָ��һ��ֵ
  long lEvent   //ָ����Щ֪ͨ����Ҫ����
                //FD_READ�����Զ��׽���
                //FD_WRITE������д�׽���
                //FD_ACCEPT�������׽��������ӽ���
                //FD_CONNECT������׽������ӶԷ�����
                //FD_CLOSE����⵽�׽��ֶ�Ӧ�����ӱ��ر�)
�ش���������Ϣ������ע���wMsg��ͬ��wParam�����׽��־��
lParamͨ��WSAGETSELECTEVENTת������FD_READ��FD_WRITE��FD_ACCEPT��FD_CONNET��FD_CLOSE}
end;

//procedure CMDeferFree(var Message); message CM_DEFERFREE;
//��ӦCM_DEFERFREE��Ϣ
procedure TCustomWinSocket.CMDeferFree(var Message);
begin
  Free;
end;

//PostMessage�첽������Ϣ
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
{WSAAsyncSelectģ������Ӧ�ó�����Windows��Ϣ�ķ�ʽ���������¼�֪ͨ����������Ҫ�󲻸ߵ�����Ӧ�ó��򶼲���WSAAsyncSelectģ��
WSAAsyncSelect�Զ����׽�������Ϊ������ģʽ������Ϊ�׽��ְ�һ�����ھ�������������¼�����ʱ������������ڷ�����Ϣ
int WSAAsyncSelect(
  SOCKET s,     //��Ҫ���õ��׽��־��
  HWND, hWnd,   //ָ��һ�����ھ�����׽��ֵ�֪ͨ��Ϣ���������˴�����
  u_int wMsg,   //�����¼�������ID��������WM_USER������ֵ������ָ��һ��ֵ
  long lEvent   //ָ����Щ֪ͨ����Ҫ����
                //FD_READ�����Զ��׽���
                //FD_WRITE������д�׽���
                //FD_ACCEPT�������׽��������ӽ���
                //FD_CONNECT������׽������ӶԷ�����
                //FD_CLOSE����⵽�׽��ֶ�Ӧ�����ӱ��ر�
)
�ش���������Ϣ������ע���wMsg��ͬ��wParam�����׽��־��
lParamͨ��WSAGETSELECTEVENTת������FD_READ��FD_WRITE��FD_ACCEPT��FD_CONNET��FD_CLOSE}

  //��FSocket��ע��CM_SOCKETMESSAGE��Ϣ
  //������Ϣ��ʱ��ͨ����ϢCM_SOCKETMESSAGE֪ͨWnd��Ӧ�Ĵ��壬��Ӧ�Ĵ���Wnd��ע��CM_SOCKETMESSAGE��Ϣȥ��Ӧ����
  //ע���֪ͨ����FAsyncStyles����
  WSAAsyncSelect(FSocket, Wnd, Msg, Longint(Byte(FAsyncStyles)));

  //����첽��ʽ����Ϊ�գ���ʹ��ioctlsocket�������������SocketΪ����ģʽ
  if FASyncStyles = [] then
  begin
    Blocking := 0;
    {ioctlsocket(
      SOCKET s,         //�׽���������
      long cmd,         //���׽��ֵĲ�������
      u_long FAR* argp  //ָ��cmd��������������ָ��
    )
    ��������������һ״̬�������׽��֣������ڻ�ȡ���׽�����صĲ����������������Э���ͨѶ��ϵͳ�޹�
    ���FIONBIO��������ֹ�׽���S�ķ�����ģʽ��argpָ��һ���޷��ų�����
    �����������ģʽ����㣬���ֹ������ģʽ��Ϊ��
    ������BlockingΪ0����ʾ����Ϊ����ģʽ}
    ioctlsocket(FSocket, FIONBIO, Blocking);
  end;
end;

procedure TCustomWinSocket.DoListen(QueueSize: Integer);
begin
  //��Socket�󶨵���Ӧ�Ķ˿��ϣ����Ҽ�鷵��ֵ
  CheckSocketResult(bind(FSocket, FAddr, SizeOf(FAddr)), 'bind');

  //�����첽ģʽ���Ƿ������������������ͨ��WSAAsyncSelectȥע����Ӧ��Щ��Ϣ
  DoSetASyncStyles;

  //SOMAXCONN       = 5;
  if QueueSize > SOMAXCONN then QueueSize := SOMAXCONN;

  //�ص�����seListen�¼�����
  Event(Self, seListen);

  //����FSocket�׽���
  //listen�ĵڶ��������ǵȴ����Ӷ��е���󳤶�
  //����˵����������Ϊ10������15�����������ʱ��ǰ10���������󱻷��õ���������У�����5�����󱻾ܾ�
  CheckSocketResult(Winsock.listen(FSocket, QueueSize), 'listen');
  FLookupState := lsIdle;
  FConnected := True;
end;

procedure TCustomWinSocket.DoOpen;
begin
  //�����첽ģʽ���Ƿ������������������ͨ��WSAAsyncSelectȥע����Ӧ��Щ��Ϣ
  DoSetASyncStyles;

  //�ص�����seConnecting�¼��ĺ���
  Event(Self, seConnecting);

  //����connect�������ӣ����Ҽ�鷵�صĽ��ֵ��
  CheckSocketResult(WinSock.connect(FSocket, FAddr, SizeOf(FAddr)), 'connect');

  FLookupState := lsIdle;

  //����������Բ����첽��ʽ�����У�Ҳ������������Ϊ����ģʽ
  if not (asConnect in FAsyncStyles) then
  begin
    //���ӳɹ�������£��ص�����seConnect�¼��ĺ���
    FConnected := FSocket <> INVALID_SOCKET;
    Event(Self, seConnect);
  end;
end;

{WndProc���������������ӵ�
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
    {AllocateHWnd����һ����ĳ���ڻ��ռ��޹���ִ��Method����ָ�����ڹ��̴���
    һ�����ڴ����ǿ��ӻ����ڣ�������Ӧδ�������û������ϵ���Ϣ}
    FHandle := AllocateHwnd(WndProc);
  Result := FHandle;
end;

//��ȡ������IP ��ַ
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
    //getsockname()�������ڻ�ȡһ���׽��ֵ����֣�������һ���Ѱ󶨻������ӵ��׽���
    //���ص�ַ��������
    if getsockname(FSocket, SockAddrIn, Size) = 0 then
      //��������һ����in��������ʾ��Internet��ַ�ṹת�����ԡ�.�� ��������硰a.b.c.d�����ַ�����ʽ
      Result := inet_ntoa(SockAddrIn.sin_addr);
  finally
    Unlock;
  end;
end;

//��ȡ����Host����
function TCustomWinSocket.GetLocalHost: string;
var
  LocalName: array[0..255] of Char;
begin
  Lock;
  try
    Result := '';
    if FSocket = INVALID_SOCKET then Exit;
    //gethostname�����ر��������ı�׼������
    if gethostname(LocalName, SizeOf(LocalName)) = 0 then
      Result := LocalName;
  finally
    Unlock;
  end;
end;

//��ȡ����������IP��ַ
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
      //��һ��16λ�����ֽ���ת��Ϊ�����ֽ���
      Result := ntohs(SockAddrIn.sin_port);
  finally
    Unlock;
  end;
end;

//��ȡ�Է�������Host����
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
    //getpeername����ȡsocket�ĶԷ���ַ��SockAddrIn�а���IP���˿���Ϣ��
    CheckSocketResult(getpeername(FSocket, SockAddrIn, Size), 'getpeername');
    //gethostbyaddr�����ݵ�ַ��ȡ��Ӧ��Host������Ϣ
    HostEnt := gethostbyaddr(@SockAddrIn.sin_addr.s_addr, 4, PF_INET);
    if HostEnt <> nil then Result := HostEnt.h_name;
  finally
    Unlock;
  end;
end;

//��ȡ�Է���IP��ַ
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

//��ȡ�Է��Ķ˿���Ϣ
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

//��ȡ�Է���IP���˿���Ϣ
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

//Lookup������ɶ��
function TCustomWinSocket.LookupName(const Name: string): TInAddr;
var
  HostEnt: PHostEnt;
  InAddr: TInAddr;
begin
  //gethostbyname�����ض�Ӧ�ڸ����������İ����������͵�ַ��Ϣ��hostent�ṹָ��
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
  //getservbyname�������������������Ӧ�İ������ֺͷ������Ϣ��servent�ṹָ��
  ServEnt := getservbyname(PChar(Service), 'tcp');
  if ServEnt <> nil then
    Result := ntohs(ServEnt.s_port)
  else Result := 0;
end;

//��ʼ��Socket�ĵ�ַ��Ϣ
{sockaddr_in = record
    case Integer of
      0: (sin_family: u_short;             //Э���壬��Socket�����ֻ����AF_INET
          sin_port: u_short;               //�洢�˿ںţ�ʹ�������ֽ���
          sin_addr: TInAddr;               //�洢IP��ַ��ʹ��id_addr������ݽṹ
          sin_zero: array[0..7] of Char);  //��Ϊ����sockaddr��sockaddr_in�������ݽṹ���ִ�С��ͬ�������Ŀ��ֽ�
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

  //����һ��Socket
  FSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  
  //sCannotCreateSocket = 'Can''t create new socket';
  if FSocket = INVALID_SOCKET then raise ESocketError.CreateRes(@sCannotCreateSocket);

  try
    //�ص�seLookup�¼�������
    Event(Self, seLookUp);
    if Block then
    begin
      //�����������ģʽ�������InitSocket������ʼ��Socket�ĵ�ַ��Ϣ
      FAddr := InitSocket(Name, Address, Service, Port, False);
      //Ȼ�����listen����
      DoListen(QueueSize);
    end else
    begin
      //����Ƿ�����ģʽ�������AsyncInitSocket��ʼ��Socket�ĵ�ַ��Ϣ
      //AsyncInitSocket��������Ƚϸ��ӣ�Ŀǰ��û�кܶ�
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

  //����һ��Socket
  FSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);

  //�ж�Socket�Ƿ񴴽��ɹ�
  if FSocket = INVALID_SOCKET then raise ESocketError.CreateRes(@sCannotCreateSocket);
  
  try
    //�ص�seLookup�¼�������
    Event(Self, seLookUp);
    if Block then
    begin
      //���������ģʽ�������InitSocket��ʼ��
      FAddr := InitSocket(Name, Address, Service, Port, True);
      //Ȼ��Open Socket
      DoOpen;
    end else
    begin
      //����Ƿ�����ģʽ�������AsyncInitSocket��ʼ��
      AsyncInitSocket(Name, Address, Service, Port, 0, True);
    end;
  except
    Disconnect(FSocket);
    raise;
  end;
end;

//�Ͽ�����
procedure TCustomWinSocket.Disconnect(Socket: TSocket);
begin
  Lock;
  try
    //WSACancelASyncRequest��������ȡ��һ���첽����
    //���첽��������һ��WSAAsyncGetXByY��������WSAAsyncGetHostByName()������
    if FLookupHandle <> 0 then
      CheckSocketResult(WSACancelASyncRequest(FLookupHandle), 'WSACancelASyncRequest');

    FLookupHandle := 0;
    if (Socket = INVALID_SOCKET) or (Socket <> FSocket) then exit;

    //�ص�seDisconnect�¼�������
    Event(Self, seDisconnect);

    //�ر�һ��Socket�����Ҽ�鷵��ֵ
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

{CallWindowProc�����ǽ���Ϣ���͸�ָ���Ĵ��ڹ���
CallWindowProc�Ĳ���
  pPrevWndFunc��ָ��ǰһ�����ڹ��̵�ָ�롣�����ֵ��ͨ������GetWindowLong�����������ú����е�nlndex������ΪGWL_WNDPROC��DWL_DLGPROC���õ��ģ���ô��ʵ����Ҫô�Ǵ��ڻ��߶Ի���ĵ�ַ��Ҫô���Ǵ���õ�ַ�ľ����
  hWnd��ָ�������Ϣ�Ĵ��ڹ��̵ľ����
  Msg��ָ����Ϣ���͡�
  wParam��ָ������ġ���Ϣ�ض�����Ϣ���ò�����������Msg����ֵ�йء�
  IParam��ָ������ġ���Ϣ�ض�����Ϣ���ò�����������Msg����ֵ�йء�}
procedure TCustomWinSocket.DefaultHandler(var Message);
begin
  with TMessage(Message) do
    if FHandle <> 0 then
      Result := CallWindowProc(@DefWindowProc, FHandle, Msg, wParam, lParam);
end;

//�ص���ͬ���¼�������
//TSocketEvent = (seLookup, seConnecting, seConnect, seDisconnect, seListen, seAccept, seWrite, seRead);
procedure TCustomWinSocket.Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
begin
  if Assigned(FOnSocketEvent) then FOnSocketEvent(Self, Socket, SocketEvent);
end;

//�ص���ͬ�Ĵ���������ErrorEvent��
//TErrorEvent = (eeGeneral, eeSend, eeReceive, eeConnect, eeDisconnect, eeAccept, eeLookup);
procedure TCustomWinSocket.Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  if Assigned(FOnErrorEvent) then FOnErrorEvent(Self, Socket, ErrorEvent, ErrorCode);
end;

//�����ı���Ϣ
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
      //���Socket�Ƿ�������û�н��������ӣ���ֱ���˳�
      if (FSocket = INVALID_SOCKET) or (not FConnected) then exit;

      while True do
      begin
        StartPos := FSendStream.Position;
        //Buffer: array[0..4095] of Byte;
        //����������ÿ�ζ�4096�ֽڵ�����
        AmountInBuf := FSendStream.Read(Buffer, SizeOf(Buffer));
        if AmountInBuf > 0 then
        begin
          //����SOCKAPI��send��������
          AmountSent := send(FSocket, Buffer, AmountInBuf, 0);

          //�жϷ���ֵ
          if AmountSent = SOCKET_ERROR then
          begin
            ErrorCode := WSAGetLastError;
            //WSAEWOULDBLOCK��10035������˼��Output Buffer�Ѿ����ˣ��޷���д������
            if ErrorCode <> WSAEWOULDBLOCK then
            begin
              //�ص�����ʱ���ִ����Ӧ�Ĵ�����
              Error(Self, eeSend, ErrorCode);

              //�Ͽ�����
              Disconnect(FSocket);

              //�ͷ�������
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

//����AStream���е�����
function TCustomWinSocket.SendStream(AStream: TStream): Boolean;
begin
  Result := False;
  if FSendStream = nil then
  begin
    FSendStream := AStream;
    Result := SendStreamPiece;
  end;
end;

//����
function TCustomWinSocket.SendStreamThenDrop(AStream: TStream): Boolean;
begin
  FDropAfterSend := True;
  Result := SendStream(AStream);
  if not Result then FDropAfterSend := False;
end;

//���Ͷ���������
//�� http://www.xumenger.com/Delphi-binary-socket-20161222/ ������ϸ��˵��
function TCustomWinSocket.SendBuf(var Buf; Count: Integer): Integer;
var
  ErrorCode: Integer;
begin
  Lock;
  try
    Result := 0;
    if not FConnected then Exit;

    //����send������������
    Result := send(FSocket, Buf, Count, 0);
    if Result = SOCKET_ERROR then
    begin
      ErrorCode := WSAGetLastError;

      //������ǻ��������˵��µĴ�����ص���Ӧ�ķ��ʹ�������
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

  //�ص�������ʵ�ֵ�seRead�¼�������
  //һ����������ڿ�����ʵ�ֵ�OnRead����
  Event(Self, seRead);
end;

//�ӽ��ջ������ж�ȡCount�ֽڵ�����
function TCustomWinSocket.ReceiveBuf(var Buf; Count: Integer): Integer;
var
  ErrorCode: Integer;
begin
  Lock;
  try
    Result := 0;

    //��ConutΪ-1ʱ����ReceiveLength��������ȡ���ջ�������Ŀǰ���е�����
    if (Count = -1) and FConnected then
      //ioctlsocket�ǿ����׽��ֵ�ģʽ
      //�ڶ�������FIONREAD��ȷ���׽����Զ������������
      //���ڶ�����ΪFIONREADʱ����������������ioctlsocket�ķ���ֵ
      //���s��SOCKET_STREAM���ͣ���FIONREAD������һ��recv()�������յ���������������ͨ�����׽ӿ����Ŷӵ�����������ͬ
      //���S��SOCK_DGRAM �ͣ���FIONREAD�����׽ӿ����Ŷӵĵ�һ�����ݱ���С
      ioctlsocket(FSocket, FIONREAD, Longint(Result))
    else begin
      if not FConnected then Exit;

      //����recv�������ӽ��ջ������ж�ȡ����
      Result := recv(FSocket, Buf, Count, 0);

      //�ж�recv�ķ���ֵ
      if Result = SOCKET_ERROR then
      begin
        ErrorCode := WSAGetLastError;
        //WSAEWOULDBLOCK��10035������˼��Output Buffer�Ѿ����ˣ��޷���д������
        if ErrorCode <> WSAEWOULDBLOCK then
        begin
          //�ص�eeReveive��Ӧ�Ĵ�����
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

//���ص�ǰ�������е���������
function TCustomWinSocket.ReceiveLength: Integer;
begin
  Result := ReceiveBuf(Pointer(nil)^, -1);
end;

//��ȡ��ǰ���ջ����������е����ݣ��������ı��ַ����ķ�ʽ����
function TCustomWinSocket.ReceiveText: string;
begin
  //�����ַ���Result�ĳ���Ϊ��ǰ���ջ��������������С
  SetLength(Result, ReceiveBuf(Pointer(nil)^, -1));
  //�ӵ�ǰ�Ľ��ջ������ж�ȡ��Ӧ����������
  SetLength(Result, ReceiveBuf(Pointer(Result)^, Length(Result)));
end;

procedure TCustomWinSocket.WndProc(var Message: TMessage);
begin
  try
    //�����ض�����Ϣ�ַ������ʵ���Ϣ������
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

//����CM_LOOKUPCOMPLETE��Ϣ
//Lookup�����Ǵ���ɶ��
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
//�ͻ��˵�Socket
procedure TClientWinSocket.Connect(Socket: TSocket);
begin
  FConnected := True;
  Event(Self, seConnect);
end;

//���ÿͻ�����������
procedure TClientWinSocket.SetClientType(Value: TClientType);
begin
  if Value <> FClientType then
    if not FConnected then
    begin
      FClientType := Value;
      if FClientType = ctBlocking then
      begin
        //���������ģʽ�����첽��ʽ����Ϊ��
        ASyncStyles := []
      end
      else
      begin
        //����Ƿ�����ģʽ�������д�����ӡ��ر��¼������첽��
        ASyncStyles := [asRead, asWrite, asConnect, asClose];
      end
    end else raise ESocketError.CreateRes(@sCantChangeWhileActive);
end;

{ TServerClientWinsocket }
//����˶�Ӧ�ͻ������ӵ�Socket

constructor TServerClientWinSocket.Create(Socket: TSocket; ServerWinSocket: TServerWinSocket);
begin
  FServerWinSocket := ServerWinSocket;
  if Assigned(FServerWinSocket) then
  begin
    FServerWinSocket.AddClient(Self);           //����Socket��ӵ�����˵����ӹ���������

    //��������Socket�Ƿ�����ģʽ
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
    FServerWinSocket.RemoveClient(Self);      //�ӷ���˵Ĺ���������ɾ����Socket
  inherited Destroy;
end;

{ TServerWinSocket }
//����˼���Socket
constructor TServerWinSocket.Create(ASocket: TSocket);
begin
  FConnections := TList.Create;          //�����ͻ������ӹ�������
  FActiveThreads := TList.Create;        //�����ͻ������Ӷ�Ӧ�Ĵ����̹߳�������
  FListLock := TCriticalSection.Create;
  inherited Create(ASocket);
  FAsyncStyles := [asAccept];            //�����տͻ��������¼�����Ϊ�첽��ʽ
end;

destructor TServerWinSocket.Destroy;
begin
  inherited Destroy;
  FConnections.Free;
  FActiveThreads.Free;
  FListLock.Free;
end;

//�����ӹ������������һ������Socket
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

//�����ӹ���������ɾ��һ���ͻ�������Socket
procedure TServerWinSocket.RemoveClient(AClient: TServerClientWinSocket);
begin
  FListLock.Enter;
  try
    FConnections.Remove(AClient);
  finally
    FListLock.Leave;
  end;
end;

//���̹߳������������һ���ͻ������Ӵ����߳�
procedure TServerWinSocket.AddThread(AThread: TServerClientThread);
begin
  FListLock.Enter;
  try
    if FActiveThreads.IndexOf(AThread) < 0 then
    begin
      FActiveThreads.Add(AThread);

      //�жϵ�ǰ�̹߳��������е��̸߳���
      if FActiveThreads.Count <= FThreadCacheSize then
        AThread.KeepInCache := True;
    end;
  finally
    FListLock.Leave;
  end;
end;

//���̹߳���������ɾ��һ���߳�
procedure TServerWinSocket.RemoveThread(AThread: TServerClientThread);
begin
  FListLock.Enter;
  try
    FActiveThreads.Remove(AThread);
  finally
    FListLock.Leave;
  end;
end;

//�ͻ����¼�������
procedure TServerWinSocket.ClientEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
begin
  //����������¼���Lookup�¼������������¼��������¼��򲻴���
  case SocketEvent of
    seAccept,
    seLookup,
    seConnecting,
    seListen:
      begin end;

    seConnect: ClientConnect(Socket);       //����������¼�����ص�ClientConnect��������
    seDisconnect: ClientDisconnect(Socket); //ͬ�ϵ��߼�
    seRead: ClientRead(Socket);
    seWrite: ClientWrite(Socket);
  end;
end;

//�ͻ��˴����¼�������
procedure TServerWinSocket.ClientError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  ClientErrorEvent(Socket, ErrorEvent, ErrorCode);
end;

//��ȡ��ǰ���ӹ��������еĶ������
function TServerWinSocket.GetActiveConnections: Integer;
begin
  Result := FConnections.Count;
end;

//��ȡ���ӹ��������еĵ�Index������
function TServerWinSocket.GetConnections(Index: Integer): TCustomWinSocket;
begin
  Result := FConnections[Index];
end;

//��ȡ��ǰ�̹߳��������С����š����̵߳ĸ���
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

//��ȡ��ǰ�̹߳��������С����ˡ����̵߳ĸ���
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

//����˽��տͻ�������
procedure TServerWinSocket.Accept(Socket: TSocket);
var
  ClientSocket: TServerClientWinSocket;
  ClientWinSocket: TSocket;
  Addr: TSockAddrIn;
  Len: Integer;
  OldOpenType, NewOpenType: Integer;
begin
  Len := SizeOf(OldOpenType);
  //getsockopt() �������ڻ�ȡ�������͡�����״̬�׽��ֵ�ѡ��ĵ�ǰֵ
  {int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t*optlen);
    sockfd��һ����ʶ�׽ӿڵ������֡�
    level��ѡ���Ĳ�Ρ����磬֧�ֵĲ����SOL_SOCKET��IPPROTO_TCP��
    optname�����ȡ���׽ӿ�ѡ�
    optval��ָ�룬ָ���������ѡ��ֵ�Ļ�������
    optlen��ָ�룬ָ��optval�������ĳ���ֵ}
  if getsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, PChar(@OldOpenType),
    Len) = 0 then
  try
    if FServerType = stThreadBlocking then
    begin
      NewOpenType := SO_SYNCHRONOUS_NONALERT;
      //setsockopt()�������ڶ��������͡�����״̬�׽�������ѡ��
      setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, PChar(@NewOpenType), Len);
    end;
    Len := SizeOf(Addr);

    //���տͻ������ӣ�Ϊ�ÿͻ�������һ��Socket
    ClientWinSocket := WinSock.accept(Socket, @Addr, @Len);
    
    if ClientWinSocket <> INVALID_SOCKET then
    begin
      //����Socket����һ��TServerClientWinSocket����
      ClientSocket := GetClientSocket(ClientWinSocket);
      if Assigned(FOnSocketEvent) then
        //�ص�seAccept�¼�������
        FOnSocketEvent(Self, ClientSocket, seAccept);

      if FServerType = stThreadBlocking then
      begin
        //���������Ƿ�����ģʽ����Ϊÿ���ͻ��˻��һ��ר�ŵĴ����߳�
        ClientSocket.ASyncStyles := [];
        GetServerThread(ClientSocket);
      end;
    end;
  finally
    Len := SizeOf(OldOpenType);
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, PChar(@OldOpenType), Len);
  end;
end;

//�Ͽ�����
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

//ΪTServerClientWinSocket����һ����Ӧ�Ĵ����߳�
function TServerWinSocket.DoCreateThread(ClientSocket: TServerClientWinSocket): TServerClientThread;
begin
  Result := TServerClientThread.Create(False, ClientSocket);
end;

//�����˿ڵȴ��ͻ�������
procedure TServerWinSocket.Listen(var Name, Address, Service: string; Port: Word;
  QueueSize: Integer);
begin
  inherited Listen(Name, Address, Service, Port, QueueSize, ServerType = stThreadBlocking);

  //���������ģʽ����ר�Ŵ���һ�����ܿͻ������ӵ��߳�
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

//�����̳߳���������̵߳�������
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

//�ڷ���˸���accept���ص�Socket������һ����ʾ�ͻ������ӵ�TServerClientWinSocket����
function TServerWinSocket.GetClientSocket(Socket: TSocket): TServerClientWinSocket;
begin
  Result := nil;
  if Assigned(FOnGetSocket) then FOnGetSocket(Self, Socket, Result);
  if Result = nil then
    Result := TServerClientWinSocket.Create(Socket, Self);
end;

//�ص��߳̽����¼�������
procedure TServerWinSocket.ThreadEnd(AThread: TServerClientThread);
begin
  if Assigned(FOnThreadEnd) then FOnThreadEnd(Self, AThread);
end;

//�ص��߳̿�ʼ�¼�������
procedure TServerWinSocket.ThreadStart(AThread: TServerClientThread);
begin
  if Assigned(FOnThreadStart) then FOnThreadStart(Self, AThread);
end;

//ΪTServerClientWinSocket���һ��ר�ŵ��߳�
//�����ǰ�̹߳����������С����ˡ����̣߳������¼�����߳�ȥ�����µ�TServerClientWinSocket
//���򴴽�һ���µ��߳�
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

//��ȡ������ClientSocket���߳�
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

//������յ��ͻ������ӣ��ص�FOnClientConnect
procedure TServerWinSocket.ClientConnect(Socket: TCustomWinSocket);
begin
  if Assigned(FOnClientConnect) then FOnClientConnect(Self, Socket);
end;

//�ͻ��˶Ͽ����ӣ��ص�FOnClientDisconnect
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
//ר���ڷ���˽��ܿͻ������ӵ��߳�
constructor TServerAcceptThread.Create(CreateSuspended: Boolean; ASocket: TServerWinSocket);
begin
  FServerSocket := ASocket;
  inherited Create(CreateSuspended);
end;

procedure TServerAcceptThread.Execute;
begin
  //FServerSocket.Accept���ڷ���˽��տͻ�������
  //��������̳߳���ѭ���Խ��տͻ��˵�����
  while not Terminated do
    FServerSocket.Accept(FServerSocket.SocketHandle);
end;

{ TServerClientThread }
//ר���ڷ���˴����Ӧ�ͻ������ӵĺ���
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
  //�߳̿�ʼʱ�Ȼص�OnThreadStart()����
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
    //�߳̽���ʱ�ص�OnThreadEnd()����
    FServerSocket.ThreadEnd(Self);     
  end;
end;

//��Ҫ���߳�ѭ���Դ����Ӧ�Ŀͻ�������
procedure TServerClientThread.ClientExecute;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
begin
  while not Terminated and ClientSocket.Connected do
  begin
    //���FDSet����
    FD_ZERO(FDSet);
    //���ÿͻ���Socket����FDSet������
    FD_SET(ClientSocket.SocketHandle, FDSet);
    TimeVal.tv_sec := 0;
    TimeVal.tv_usec := 500;
    //ͨ��select������FDSet����Ϊ�������¼���Socket����
    //�ȴ�TimeVal�������пɶ���Socket��Ȼ��FDSet�����У����Դ�����������е�����Socketȥ������
    //����DoRead����ȥ��
    if (select(0, @FDSet, nil, nil, @TimeVal) > 0) and not Terminated then
      if ClientSocket.ReceiveBuf(FDSet, -1) = 0 then Break
      else Synchronize(DoRead);
    //ͨ��select������FDSet����Ϊ����д�¼���Socket����
    //�ȴ�TimeVal�������п�д��Socket��Ȼ��FDSet�����У����Դ�����������е�����Socketȥд����
    //����DoWrite����ȥд
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

//�ص���Ӧ���¼�������
procedure TAbstractSocket.DoEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
begin
  Event(Socket, SocketEvent);
end;

//�ص���Ӧ�Ĵ�������
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

//��ʼ��Socket
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

//����IP��ַ
procedure TAbstractSocket.SetAddress(Value: string);
begin
  if CompareText(Value, FAddress) <> 0 then
  begin
    if not (csLoading in ComponentState) and FActive then
      raise ESocketError.CreateRes(@sCantChangeWhileActive);
    FAddress := Value;
  end;
end;

//����Host����
procedure TAbstractSocket.SetHost(Value: string);
begin
  if CompareText(Value, FHost) <> 0 then
  begin
    if not (csLoading in ComponentState) and FActive then
      raise ESocketError.CreateRes(@sCantChangeWhileActive);
    FHost := Value;
  end;
end;

//���ö˿�
procedure TAbstractSocket.SetPort(Value: Integer);
begin
  if FPort <> Value then
  begin
    if not (csLoading in ComponentState) and FActive then
      raise ESocketError.CreateRes(@sCantChangeWhileActive);
    FPort := Value;
  end;
end;

//����
procedure TAbstractSocket.SetService(Value: string);
begin
  if CompareText(Value, FService) <> 0 then
  begin
    if not (csLoading in ComponentState) and FActive then
      raise ESocketError.CreateRes(@sCantChangeWhileActive);
    FService := Value;
  end;
end;

//����
procedure TAbstractSocket.Open;
begin
  Active := True;
end;

//�ر�
procedure TAbstractSocket.Close;
begin
  Active := False;
end;

{ TCustomSocket }
//���ݲ�ͬ���¼���ȥ�ص������߶�Ӧʵ�ֺ�ע��Ļص�����
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

//���ݲ�ͬ�Ĵ���ȥ�ص������߶�Ӧʵ�ֺ�ע��Ļص�����
procedure TCustomSocket.Error(Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  if Assigned(FOnError) then FOnError(Self, Socket, ErrorEvent, ErrorCode);
end;

{ TWinSocketStream }
//Socket������
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

//�ȴ��ɶ�
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

//��Buffer��ʼ��ȡCount�ֽڵ�����
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

//��Buffer��д��Count�ֽڵ�����
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
//�ͻ���Socket������
constructor TClientSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientSocket := TClientWinSocket.Create(INVALID_SOCKET);      //����һ��TClientWinSocket�������ڽ���ͨ��
  InitSocket(FClientSocket);                                     //��ʼ��TClientWinSocket����
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

//��ȡ�ͻ������ͣ����� or ������
function TClientSocket.GetClientType: TClientType;
begin
  Result := FClientSocket.ClientType;
end;

//���ÿͻ�������
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
//�����Socket������
constructor TServerSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerSocket := TServerWinSocket.Create(INVALID_SOCKET);
  InitSocket(FServerSocket);
  FServerSocket.ThreadCacheSize := 10;
end;

end.
