#include <WinSock2.h>
#include <Windows.h>
#include <vector>
#include <iostream>

using namespace std;

#pragma comment(lib, "Ws2_32.lib")		//Socket编程需要使用的动态链接库
#pragma comment(lib, "Kernel32.lib")	//IOCP需要用到的动态链接库

/*
* 结构体名称：PER_IO_DATA，重叠结构
* 结构体功能：重叠IO需要用到的结构体，临时记录IO数据
*/
const int DataBuffSize = 2 * 1024;
typedef struct
{
	/*typedef struct _OVERLAPPED { 
	　　DWORD Internal;		//预留给OS使用，指定一个独立于系统的状态，当GetOverlappedResult函数返回时没有设置扩展错误信息ERROR_IO_PENDING时有效。
	　　DWORD InternalHigh; //预留给OS使用，指定长度的数据转移，当GetOverlappedResult函数返回TRUE时有效
	　　DWORD Offset;		//该文件的位置是从文件起始处的字节偏移量，调用进程设置这个成员之前调用ReadFile或WriteFile函数。当读取或写入命名管道和通信设备时这个成员被忽略设为零
	　　DWORD OffsetHigh;	//指定文件传送的字节偏移量的高位字。当读取或写入命名管道和通信设备时这个成员被忽略设为零
	　　HANDLE hEvent;		//在转移完成时处理一个事件设置为有信号状态。调用进程集这个成员在调用ReadFile、 WriteFile、TransactNamedPipe、 ConnectNamedPipe函数之前
	　　} OVERLAPPED
	*/
	OVERLAPPED overlapped;
	/*用来接收WSASocket数据的缓冲
	typedef struct __WSABUF
	{
		u_long len;
		char FAR *buf;
	}WSABUF, *LPWSABUF;
	较多情况下用于完成端口，从系统的内核中获取WSASocket数据
	可以设置len的大小来控制你想要获得len长度的数据
	可以使用完成端口的GetQueuedCompletionStatus获取数据
	*/
	WSABUF databuff;
	char buffer[DataBuffSize];	//const int DataBuffSize = 2 * 1024;
	int BufferLen;				//缓冲大小
	int operationType;			//标志这个重叠I/O操作是做什么的，例如Accept/Recv等  
}PER_IO_OPERATION_DATA, *LPPER_IO_OPERATION_DATA, *LPPER_IO_DATA, PER_IO_DATA;

/*
* 结构体名称：PER_HANDLE_DATA
* 结构体存储：记录单个套接字的数据，包括了套接字的变量及套接字的对应的客户端地址
* 结构体作用：当服务器连接上客户端时，信息存储到该结构体中，知道客户端的地址以便于回访
*/
typedef struct
{
	SOCKET socket;				//socket
	SOCKADDR_IN ClientAddr;		//客户端的地址
}PER_HANDLE_DATA, *LPPER_HANDLE_DATA;

//定义全局变量
const int DefaultPort = 6000;				//服务端监听的端口号
vector < PER_HANDLE_DATA* > clientGroup;	//记录客户端的向量组

HANDLE hMutex = CreateMutex(NULL, false, NULL);			//互斥对象
DWORD WINAPI ServerWorkThread(LPVOID CompletionPortID);	//服务端工作线程
DWORD WINAPI ServerSendThread(LPVOID ipPrarm);			//服务端发送线程

//开始主函数
int main()
{
	//加载Socket动态链接库
	WORD wVersionRequested = MAKEWORD(2, 2);	//请求2.2版本的WinSock库
	WSADATA wsaData;							//接收Windows Socket的结构信息
	DWORD err = WSAStartup(wVersionRequested, &wsaData);

	//检查套接字库是否申请成功
	if(0 != err){
		cerr << "Request Windows Socket Library Error!\n";
		system("pause");
		return -1;
	}
	//检查是否申请了所需版本的套接字库
	if(LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2){
		WSACleanup();
		cerr << "Request Windows Socket Version 2.2 Error!\n";
		system("pause");
		return -1;
	}

//创建IOCP的内核函数
	/* 需要用到的函数原型：
	 * HANDLE WINAPI CreateIoCompletionPort(
	 *		_in HANDLE FileHandle,				//已经打开的文件句柄或空句柄，一般是客户端的句柄
	 *		_in HANDLE ExistingCompletionPort,	//已经存在的IOCP句柄
	 *		_in ULONG_PTR CompletionKey,		//完成键，包含了指定IO完成包指向的文件
	 *		_in DWORD NumberOfConcurrentThreads	//真正并发同时执行最大线程数，一般推介是CPU核心数*2
	 *	);
	 * 第一次调用CreateIoCompletionPort用于创建一个IOCP句柄
	 * 后续调用CreateIoCompletionPort用于将socket和IOCP句柄进行关联
	 */
	HANDLE completionPort = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
	if(NULL == completionPort){//创建IO内核对象失败
		cerr << "CreateIoCompletionPort Failed. Error: " << GetLastError() << endl;
		system("pause");
		return -1;
	}

//创建IOCP线程：线程里面创建线程池
	//确定处理器的核心数量
	SYSTEM_INFO mySysInfo;
	GetSystemInfo(&mySysInfo);

	//基于处理器的核心数量创建线程（线程数推荐为核心数的2倍）
	for(DWORD i=0; i<(mySysInfo.dwNumberOfProcessors*2); i++){
		//创建服务器工作器线程，并将完成端口传递到该线程
		HANDLE ThreadHandle = CreateThread(NULL, 0, ServerWorkThread, completionPort, 0, NULL);
		if(NULL == ThreadHandle){
			cerr << "Create Thread Handle failed. Error:" << GetLastError() << endl;  
			system("pause");  
			return -1;  
		}
		CloseHandle(ThreadHandle);
	}

//建立流式套接字
	SOCKET srvSocket = socket(AF_INET, SOCK_STREAM, 0);

//绑定SOCKET到本机
	SOCKADDR_IN srvAddr;
	srvAddr.sin_addr.S_un.S_addr = htonl(INADDR_ANY);
	srvAddr.sin_family = AF_INET;
	srvAddr.sin_port = htons(DefaultPort);
	int bindResult = bind(srvSocket, (SOCKADDR*)&srvAddr, sizeof(SOCKADDR));
	if(SOCKET_ERROR == bindResult){
		cerr << "Bind failed. Error: " << GetLastError() << endl;
		system("pause");
		return -1;
	}

//将SOCKET设置为监听模式
	int listenResult = listen(srvSocket, 10);
	if(SOCKET_ERROR == listenResult){
		cerr << "Listen failed. Error: " << GetLastError() << endl;
		system("pause");
		return -1;
	}

//开始处理IO数据
	 cout << "本服务器已准备就绪，正在等待客户端的接入...\n";
	 
	 //创建用于发送数据的线程
	 HANDLE sendThread = CreateThread(NULL, 0, ServerSendThread, 0, 0, NULL);

	 //主线程内部循环accept客户端连接
	 //并且调用CreateIoCompletionPort将socket和IOCP句柄建立关系
	 while(true){
		/*  typedef struct
			{
				SOCKET socket;				//socket
				SOCKADDR_IN ClientAddr;		//客户端的地址
			}PER_HANDLE_DATA, *LPPER_HANDLE_DATA;			 
		该结构体用于保存客户端连接过来的socket的相关信息		*/
		PER_HANDLE_DATA * PerHandleData = NULL;
		SOCKADDR_IN saRemote;
		int RemoteLen;
		SOCKET acceptSocket;

		//接收连接，并分配完成端，这儿可以用AcceptEx()
		RemoteLen = sizeof(saRemote);
		acceptSocket = accept(srvSocket, (SOCKADDR*)&saRemote, &RemoteLen);
		if(SOCKET_ERROR == acceptSocket){//接收客户端失败
			cerr << "Accept Socket Error: " << GetLastError() << endl;
			system("pause");
			return -1;
		}

		//创建用来和套接字关联的单句柄数据信息结构
		PerHandleData = (LPPER_HANDLE_DATA)GlobalAlloc(GPTR, sizeof(PER_HANDLE_DATA));	//在堆里为这个PerHandleData申请置顶大小的内存
		PerHandleData->socket = acceptSocket;						//客户端socket
		memcpy(&PerHandleData->ClientAddr, &saRemote, RemoteLen);	//客户端地址信息
		
		//将单个客户端数据指针放到客户端组中
		clientGroup.push_back(PerHandleData);		

		//收到一个新的连接后，将这个连接的套接字和完成端口关联
		CreateIoCompletionPort((HANDLE)(PerHandleData->socket), completionPort, (DWORD)PerHandleData, 0);

		//开始在接收套接字上处理IO使用重叠IO机制
		//在新建的套接字上投递一个或多个异步WSARecv或WSASend请求，这些IO请求完成后，工作线程会为IO请求提供服务

		//单IO操作数据（IO重叠）
		/*	typedef struct
			{
				OVERLAPPED overlapped;
				WSABUF databuff;
				char buffer[DataBuffSize];	//const int DataBuffSize = 2 * 1024;
				int BufferLen;				//缓冲大小
				int operationType;			//标志这个重叠I/O操作是做什么的，例如Accept/Recv等  
			}PER_IO_OPERATION_DATA, *LPPER_IO_OPERATION_DATA, *LPPER_IO_DATA, PER_IO_DATA;			*/
		LPPER_IO_OPERATION_DATA PerIoData = NULL;
		//申请内存
		PerIoData = (LPPER_IO_OPERATION_DATA)GlobalAlloc(GPTR, sizeof(LPPER_IO_OPERATION_DATA));
		//将内存清零
		ZeroMemory(&(PerIoData->overlapped), sizeof(OVERLAPPED));
		//设置接收缓冲区的信息（大小和指针）
		PerIoData->databuff.len = 1024;
		PerIoData->databuff.buf = PerIoData->buffer;
		PerIoData->operationType = 0;		//read

		DWORD RecvBytes;
		DWORD Flags = 0;
		/* 在重叠模型中，接收数据就要靠它了，它的参数比recv要多，因为要用到重叠结构
		 * 参数1：要投递的这个操作的套接字
		 * 参数2：需要一个WSABuff的结构体数组
		 * 参数3：WSABuff结构体数组元素的数量
		 * 参数4：入股接收操作立即完成，这里会返回函数调用所接收到的字节数
		 * 参数5：lpFlags参数可以是下面任何一个值：MSG_PEEK、MSG_OOB、MSG_PARTIAL或者对这些值进行按位和运算之后的结果
		 * 参数6：
		 * 在新建的套接字上投递一个或多个异步WSARecv或WSASend请求，这些IO请求完成后，工作线程会为IO请求提供服务
		*/
		WSARecv(PerHandleData->socket, &(PerIoData->databuff), 1, &RecvBytes, &Flags, &(PerIoData->overlapped), NULL);
	}

	system("pause");
	return 0;
}
 
/* 关于IOCP模型需要特别说明一下
   * accept线程
     * 一个专门的线程用于accept客户端连接
     * 在accept线程中，将socket和完成端口进行关联
     * 在accept线程中，调用WSARecv方法，在该新的套接字上投递一个新的WSARecv请求（也可以是写操作）
     * accept线程的主要工作就是完成这个循环
   * 工作线程
     * 调用GetQueuedCompletionStatus方法阻塞等待某个socket的IO完成事件，或者超时
	 * GetQueuedCompletionStatus完成后，获得IO完成的socket
	 * 处理对应的缓冲区中的已经读取好的数据
	 * 然后再调用WSARecv在该socket上再投递一个IO请求，等待IO完成
	 * 如此循环
 * select和IOCP的很大的区别是
   * IOCP调用GetQueuedCompletionStatus获得一个socket后，内核中已经完成了对该socket的IO操作，比如已经将内核接收区的数据拷贝到指定的缓冲区中了
   * 这主要是关于读IO的描述，写的话，则是你先往用户态缓冲区中放入数据，然后调用WSASend投递一个写请求
     * 然后内核中发现发送缓冲区中可写的时候就会把数据写到接收缓冲区中
	 * 写入完成后，你可以通过GetQueuedCompletionStatus获取该事件
   * select模式则是在调用select后，获取了可读、可写的socket，然后需要自己再去调用send、recv方法去进行读写
   * IOCP是完成了IO之后通知你，你直接去处理IO好的数据
   * select是可以进行IO的时候通知你，你先去自己调用IO函数进行IO，然后再去处理IO好的数据

* 这些是关于IOCP、select的简单讲解
   * 实际的应用中，还需要结合阻塞模式的使用
   * 还需要考虑内核发送缓冲区慢了的情况
   * 还需要考虑内核接收缓冲区中数据不足导致读取的数据不完成的情况
   * 等等特殊情况还都是需要再进行考虑的
*/

//开始服务工作线程函数
DWORD WINAPI ServerWorkThread(LPVOID lpParam)
{
	HANDLE CompletionPort = (HANDLE)lpParam;
	DWORD BytesTransferred;
	LPOVERLAPPED lpOverlapped;
	/*	typedef struct
		{
			SOCKET socket;				//socket
			SOCKADDR_IN ClientAddr;		//客户端的地址
		}PER_HANDLE_DATA, *LPPER_HANDLE_DATA;	*/
	LPPER_HANDLE_DATA PerHandleData = NULL;
	/*	typedef struct
		{
			OVERLAPPED overlapped;
			WSABUF databuff;
			char buffer[DataBuffSize];	//const int DataBuffSize = 2 * 1024;
			int BufferLen;				//缓冲大小
			int operationType;			//标志这个重叠I/O操作是做什么的，例如Accept/Recv等  
		}PER_IO_OPERATION_DATA, *LPPER_IO_OPERATION_DATA, *LPPER_IO_DATA, PER_IO_DATA;			*/
	LPPER_IO_DATA PerIoData = NULL;
	DWORD RecvBytes;
	DWORD Flags = 0;
	BOOL bRet = false;

	while(true){
		/* GetQueuedCompletionStatus使调用线程挂起
		 * 直到指定的端口的IO完成队列中出现一项或直到超时

		 * GetQueuedCompletionStatus是在内核完成读写IO之后（往重叠结构指定的内存区域读写）返回
		 * 当该函数返回了一个Socket之后，IO操作已经完成了，接下来直接对IO完成的数据进行处理就好了，不需要再进行IO操作了
		 * select是通知你可以进行IO了，然后由你调用IO操作
		 * IOCP则是完成了IO之后，告诉你完成了，你直接去处理数据就好了
		 * 理解这个区别很重要
		 * 下面还应该判断到底是成功获取一个Socket还是等待超时！
		*/
		bRet = GetQueuedCompletionStatus(CompletionPort, &BytesTransferred, (unsigned long*)&PerHandleData, (LPOVERLAPPED*)&lpOverlapped, INFINITE); 
		if(0 == bRet){
			cerr << "GetQueuedCompletionStatus Error: " << GetLastError() << endl;  
			return -1; 
		}

		/* GetQueuedCompletionStatus执行成功后，
		 * 通过GetQueuedCompletionStatus的第4个参数可以获得对应的IO重叠结构PerIoData
		 * 后续使用PerIoData获取读取到的数据
		 */
		PerIoData = (LPPER_IO_DATA)CONTAINING_RECORD(lpOverlapped, PER_IO_DATA, overlapped);

		//检查套接字上是否有错误发送
		if(0 == BytesTransferred){
			closesocket(PerHandleData->socket);
			GlobalFree(PerHandleData);
			GlobalFree(PerIoData);
			continue;
		}

		//开始数据处理，接收来自客户端的数据
		WaitForSingleObject(hMutex, INFINITE);
		//通过PerIoData获取读到的数据信息！
		cout << "A Client says: " << PerIoData->databuff.buf << endl;
		ReleaseMutex(hMutex);

		//为下一个重叠调用建立单IO操作数据
		ZeroMemory(&(PerIoData->overlapped), sizeof(OVERLAPPED));	//清空内存
		PerIoData->databuff.len = 1024;
		PerIoData->databuff.buf = PerIoData->buffer;
		PerIoData->operationType = 0;		//read

		//再在这个套接字上投递一个或多个异步WSARecv或WSASend请求
		//这些IO请求完成后，工作线程会在后续的循环这种为IO请求提供服务
		WSARecv(PerHandleData->socket, &(PerIoData->databuff), 1, &RecvBytes, &Flags, &(PerIoData->overlapped), NULL);
	}
	return 0;
}

//发送信息的线程执行函数
DWORD WINAPI ServerSendThread(LPVOID lpParam)
{
	while(1){
		char talk[200];
		gets(talk);
		int len;
		for(len=0; talk[len]!='\0'; ++len){
			//算出这个字符组的长度
		}
		talk[len] = '\n';
		talk[++len] = '\0';
		printf("I Say: ");
		cout << talk;

		WaitForSingleObject(hMutex, INFINITE);
		for(int i=0; i<clientGroup.size(); ++i){
			send(clientGroup[i]->socket, talk, 200, 0);		//发送消息
		}
		ReleaseMutex(hMutex);
	}
	return 0;
}
