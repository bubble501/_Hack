# 本篇文章的背景

最近现场反馈了一个上海协议回购的转换机任务出现out of memory的问题。下面先介绍一下这个转换机任务的网络通信的背景！

首先是通信协议的介绍

```
| 固定4字节的长度信息(二进制int) | 数据内容 |
```

在程序中先读固定4字节的长度，比如是n字节，那么接下来继续读这n字节的数据内容。如此来进行数据传输

### 程序读网络包逻辑

协议回购出现out of memory的问题，通过加日志，发现读到的首4个字节竟然是达到130000甚至更大，结果在程序中调用

```
var
  msgLen: Integer;
  buf: array of Byte;
  len, index: Integer;
  x: Integer;
begin
  try
    if (0 < Socket.ReceiveLength) then
    begin
      Socket.ReceiveBuf(msgLen, 4);     //前4节，长度
      SetLength(buf, msgLen);           //申请msgLen长度的内存

      //非阻塞模式下循环读msgLen长度的内容。因为非阻塞可能不能一次读齐，需要循环读、凑齐
      len := msgLen;
      index := 0;
      while (4 * 1024 <= len) do
      begin
        Socket.ReceiveBuf(buf[index], 4 * 1024);
        index := index + 4 * 1024;
        len := len - 4 * 1024;
      end;
      if (0 < len) then
        Socket.ReceiveBuf(buf[index], len);

      //...
      //读到的buf数据再做其他用处
      //...
    else
      //在(http://www.xumenger.com/socketapi-onread-20170406/)中有讲到这样用的原因
      Socket.ReceiveBuf(x, 0);
  except
    on E: Exception do
      SendMsg(FTaskInfo, const_pubMsgError, '接收服务端信息时出现异常：' + E.Message);
  end;
end;
```

但是我们看程序中收数据是有问题的，这里调用`Socket.ReceiveBuf(buf[index], 4 * 1024);`是默认可以读到`4 * 1024`字节的，但是非阻塞模式下，可能因为接收缓冲区中不足`4 * 1024`字节，只有`1024`字节，那么实际只会读到1024字节，那么这里的逻辑明显导致有问题！

所以在非阻塞模式下必须考虑ReceiveBuf的返回值。参考深交所五代Binary网关的代码实现进行下面的修改

### 修改后的阻塞模式下读网络包的逻辑

```
//增加一个函数，在非阻塞模式下保证读固定长度的数据
function TTrader.RecvFixedLen(Socket: TCustomWinSocket; var Buf; iDataLen: Integer): Integer;
var
  iRet, iRecvLen, iTotalLen: Integer;
  cBuf: PChar;
begin
  iRet := 0;
  iRecvLen := 0;
  iTotalLen := 0;
  cBuf := @Buf;
  while(iTotalLen < iDataLen) do
  begin
    iRecvLen := iDataLen - iTotalLen;
    iRet := Socket.ReceiveBuf((cBuf + iTotalLen)^, iRecvLen);
    if (iRet > 0) then
      iTotalLen := iTotalLen + iRet;
    else if (iRet = 0) then
      //这种情况下可以认为是对方优雅的关闭了连接。暂不考虑
    else
    begin
      Result := 0;
      Exit;
    end;        
  end;
  Result := iTotalLen;
end;


//通过调用RecvFixedLen保证非阻塞模式下读到固定长度的数据
procedure TTrader.SocketRead(Sender: TObject; Socket: TCustomWinSocket);
var
  msgLen: Integer;
  aRespMsg: PRespMsg;
  buf: array of Byte;
  x: Integer;
begin
  try
    if (0 < Socket.ReceiveLength) then
    begin
      New(aRespMsg);
      //前4节，长度
      RecvFixedLen(Socket, msgLen, 4);
      //申请msgLen长度的内存
      SetLength(buf, msgLen);
      //消息主体部分
      RecvFixedLen(Socket, buf[0], msgLen);

      //...
      //读到的buf数据再做其他用处
      //...

    end
    else
      Socket.ReceiveBuf(x, 0);
  except
    on E: Exception do
      SendMsg(FTaskInfo, const_pubMsgError, '接收服务端信息时出现异常：' + E.Message);
  end;
end;
```

目前的代码逻辑是这样的，但是自己看RecvFixedLen函数中的处理方式还是有问题的，下面是其处理返回值的情况

```
    iRet := Socket.ReceiveBuf((cBuf + iTotalLen)^, iRecvLen);
    if (iRet > 0) then
    begin
      iTotalLen := iTotalLen + iRet;
    end
    else if (iRet = 0) then
    begin
      //这种情况下可以认为是对方优雅的关闭了连接
      //暂不考虑
    end
    else
    begin
      Result := 0;
      Exit;
    end;
```

目前的实现这种对于ReceiveBuf的除了`> 0`的返回值情况处理的比较完善，但是对于`= 0`和`< 0`的情况没有做很好的处理。在某些异常情况下应该会出现一些

另外还有`ReceiveLength`和`ReceiveBuf`的配合使用感觉也不是很正确

### out of memory的真实原因

上面讲到了在现场遇到了out of memory的问题，初步已经发现是收到的报文数据有问题，导致程序中申请到了过大的内存

后面排查具体的原因，是因为部署问题导致的。正常的配置应该是

```
转换机 <--> EzDataAccess <--> 交易所后台
```

但是他们现在错误部署成这样

```
转换机 <--> 交易所后台
```

上面说到的通信协议是转换机和EzDataAccess之间约定的协议，他们可以按照这个协议读写数据。但是错误的部署方式下，不敢保证交易所后台发过来什么数据，导致转换机收到错误的数据，解析到错误的结果，结果out of memory的问题就出现了

更改为正确的部署就OK了

但是这个过程让我产生了很多思考：

* 如果别人故意给你发错误的报文，现在的程序中是没有任何保护的
    * 因为这是在内网的程序，所以没有问题
    * 但是如果是在公网上，很容易被别人攻击
    * 网络安全编程的东西很值得自己思考
* 如何正确的调用Socket API读写
    * 在慢速的收发数据的情况下可能程序的问题不会被触发
    * 但是如果高并发的压测，估计会出现各种问题，这个需要如何完善程序？
* 等等

### 反思

在上面回顾自己的网络编程的代码逻辑的时候，已经指出来存在的各种问题了

另外虽然自己参考陈硕在Boolan的课程、陈硕的muduo书的时候尝试去学习网络编程，但是面对很多的网络编程的场景依然不清楚有什么完善的应对方案。而且自己目前对于网络原理的细节，尤其是TCP的各种细节掌握的很粗糙！

列出一些问题，好好思考网络编程（非阻塞模式）：

* Windows Socket API 和Linux Socket API在使用上有什么区别？
* 陈硕多次提到关闭网络连接是很难的，那么有什么好的关闭网络连接的方案？
* 参考MSDN，知道recv有`> 0`、`= 0`、`= -1`三种可能的情况，要怎么分别进行处理？
* 当出现高并发的请求、高并发的数据的时候，如何保证程序的稳定性、数据的一致性？

>现在自己对于网络原理、网络编程的细节只是一个混沌的知道状态，没有精确到每一个bit！

# API接口规范

>参考帮助文档，保证每一种可能的返回值情况都做到合理的处理。有一种可能的返回值没有处理到那么在这种返回值情况发生时，程序就会出现难以预料的问题！可想而知，这样的程序是极其不稳定的

然后推荐一些高质量的官方接口规范资源：

* [MSDN: Windows API查询](https://msdn.microsoft.com/zh-cn/)
* [cppreference: C++语法查询](http://en.cppreference.com/w/)
* Linux下有问题直接找`man`

>除了API的接口文档，还推荐其他的官方文档，比如C++的标准说明、[所有的RFC](https://tools.ietf.org/rfc/index)等等。从官方的标准中寻找开发的规则和注意事项是绝对不会出错的，

下面对于Socket API的接口进行说明，这是Windows下的Socket API的说明，Linux下的不保证如此。请针对API接口说明进行深刻的思考网络编程！

# ReceiveBuf函数定义

首先看一下ScktComp单元中ReceiveBuf函数的代码实现

```
function TCustomWinSocket.ReceiveBuf(var Buf; Count: Integer): Integer;
var
  ErrorCode: Integer;
begin
  Lock;
  try
    Result := 0;
    if (Count = -1) and FConnected then
      ioctlsocket(FSocket, FIONREAD, Longint(Result))
    else begin
      if not FConnected then Exit;
      Result := recv(FSocket, Buf, Count, 0);
      if Result = SOCKET_ERROR then
      begin
        ErrorCode := WSAGetLastError;
        if ErrorCode <> WSAEWOULDBLOCK then
        begin
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
```

其中`Result := recv(FSocket, Buf, Count, 0)`是获取返回值的地方

继续看recv的函数声明

```
function recv;              external    winsocket name 'recv';
```

然后查一下[MSDN](https://msdn.microsoft.com/zh-cn/)上的接口说明

### recv接口说明

The **recv** function recveives data from a connected socket or a bound connectionless socket

**Syntax**

```
int recv(
  _In_  SOCKET s,
  _Out_ char   *buf,
  _In_  int    len,
  _In_  int    flags
);
```

**Parameters**

* **s[in]**: The descriptor that identifies a connected socket
* **buf[out]**: A pointer to the buffer to receive the incoming data
* **len[in]**: The length, in bytes, of the buffer pointerd to by the buf parameter
* **flags[in]**: A set of flags that influences the behavior of this function. See remarks below. See the Remarks section for details on the possible value for this parameter

**Return value**

If no error occurs, **recv** returns the number of bytes received and the buffer pointed to by the buf parameter will contain this data received. If the connection has been gracefully closed, the return value is zero

Otherwise, a value of SOCKET_ERROR is returned, and a specific error code can be retrieved by calling **WSAGetLastError**

更详细的说明参见[recv的接口说明](https://msdn.microsoft.com/zh-cn/ms740121)

# 对应看一下SendBuf的说明

上面讲到了Windows Socket API中ReceiveBuf函数的接口说明，对应的，我们也去看一下SendBuf这个函数

```
function TCustomWinSocket.SendBuf(var Buf; Count: Integer): Integer;
var
  ErrorCode: Integer;
begin
  Lock;
  try
    Result := 0;
    if not FConnected then Exit;
    Result := send(FSocket, Buf, Count, 0);
    if Result = SOCKET_ERROR then
    begin
      ErrorCode := WSAGetLastError;
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
```

其中调用的是send这个SOCKET API，对应的MSDN地址在[这里](https://msdn.microsoft.com/zh-cn/library/windows/desktop/ms740149(v=vs.85).aspx)

### send接口说明

The **send** function sends data on a connected socket

**Syntax**

```
int send(
  _In_         SOCKET s
  _In_ const char *buf;
  _In_         int len,
  _In_         int flags
);
```

**Parameters**

* **s[in]**: A descriptor identifying a connected socket
* **buf[in]**: A pointer to a buffeer containing the data to the transmitted
* **len[in]**: The length, in bytes, of the data in buffer pointed to by the buf parameter
* **flags[in]**:  A set of flags that specify the way in which the call is made. This parameter is constructed by using the bitwise OR operator with any of the following values

Value          | Meaning
---------------|---------------------------
MSG_DONTROUTE  | Specifies that the data should not be subject to routing. A Windows Sockets service provider can choose to ignore this flag.
MSG_OOB        | Sends OOB data (stream-style socket such as SOCK_STREAM only.

**Return value**

if no error occers, **send** return the total number of bytes sent, which can be less than the number requested to be sent in the *len* parameter. Otherwise, a value of SOCKET_ERROR is returned, and a specific error code can be retrieved by calling WSAGetLastError.

在接口文档中说到存在要发送100Byte，实际发送出去20Byte，结果返回值是20Byte的情况。但是我在[《ScktComp非阻塞网络编程的坑》](http://www.xumenger.com/socketapi-error-usage-20170404/)对send的测试完全不是这样

>妈的，这部分又是一个网络编程需要深刻考虑的点，是什么原因导致自己的测试结果和API接口文档说明不一致？

>为什么我实际测试要么全部发送成功，要么全部失败，并没有部分发送成功的情况

>为什么实际的测试情况和接口文档的说明出现偏差？是不是自己测试的方法有问题

>我在调用recv/send返回-1时并没调WSAGetLastError查错误码，上面讲测试和接口有误差可能是因为在某种LastError下会出现的情况

所以[《ScktComp非阻塞网络编程的坑》](http://www.xumenger.com/socketapi-error-usage-20170404/)中的测试是不太完善的。没有完整的参考官方API文档

官方API接口文档的重要性再怎么强调也不为过！
