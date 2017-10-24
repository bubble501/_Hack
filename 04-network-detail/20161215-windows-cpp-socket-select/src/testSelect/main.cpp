#include<WinSock2.h>
#include<stdio.h>
#include<stdlib.h>
#include<iostream>

using namespace std;

#pragma comment(lib, "ws2_32.lib")

#define PORT 8000
//#define MSGSIZE 1024 * 1024	如果这样定义的话，会导致任务启动时栈溢出，估计和内存栈空间的一些限制有关
#define MSGSIZE 10240
#define SRV_IP "127.0.0.1"

int g_nSockConn = 0;			//请求连接的数目
CRITICAL_SECTION ConnectLock;	//临界区锁，用于保证线程安全

struct ClientInfo{
	SOCKET sockClient;		//客户端套接字信息
	SOCKADDR_IN addrClient;	//客户端地址信息
};

//FD_SETSIZE是在winsocket2.h中定义的，这里Windows默认最大是64
//在包含winsocket2.h之前使用宏定义可以修改这个值
//存储客户端连接的数组，因为是测试，所以用数组就可以搞定，生产环境可能用队列等数据结构来实现
ClientInfo g_Client[FD_SETSIZE];	

DWORD WINAPI WorkThread(LPVOID lpParameter);

/********************************************************************************************************
*********服务器的主要步骤**********
1.创建监听套接字-->绑定-->监听
2.创建工作线程
3.创建一个套接字组，用来存放当前所有活动的客户端套接字，每accept一个连接就更新一次数组
4.接收客户端的连接，因为没有重新定义FD_SIZE宏，服务器最多支持64个并发连接（最好是记录下连接数，不要无条件的接受连接）
********************************************************************************************************/
int main(int argc, char* argv[])
{
	cout << "调用WSAStartup" << endl;
	WSADATA wsaData;
	WSAStartup(MAKEWORD(2, 2), &wsaData);

	cout << "初始化临界区锁" << endl;
	InitializeCriticalSection(&ConnectLock);

	cout << "创建服务端socket" << endl;
	SOCKET sockListen = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

	SOCKADDR_IN addrSrv;
	addrSrv.sin_addr.S_un.S_addr = inet_addr(SRV_IP);
	addrSrv.sin_family = AF_INET;
	addrSrv.sin_port = htons(PORT);

	cout << "服务端socket绑定地址、端口" << endl;
	bind(sockListen, (SOCKADDR *)&addrSrv, sizeof(SOCKADDR));

	cout << "服务端socket开始监听" << endl;
	listen(sockListen, 64);

	DWORD dwThreadIDRecv = 0;
	DWORD dwThreadIDWrite = 0;

	cout << "创建用来处理收发消息的线程" << endl;
	HANDLE hand = CreateThread(NULL, 0, &WorkThread, NULL, 0, &dwThreadIDRecv);
	if(NULL == hand){
		cout << "创建工作线程失败！\n";
		getchar();
		return -1;
	}

	SOCKET sockClient;
	SOCKADDR_IN addrClient;
	int nLenAddrClient = sizeof(addrClient);
	
	cout << "开始循环" << endl;
	while(true){
		//第三个参数一定按照addrClient的大小初始化
		sockClient = accept(sockListen, (SOCKADDR *)&addrClient, &nLenAddrClient);
		
		cout << "收到一个客户端的连接" << endl;
		if(INVALID_SOCKET != sockClient){
			cout << inet_ntoa(addrClient.sin_addr) << ": " << ntohs(addrClient.sin_port) << " 连接成功！" << endl;

			if(g_nSockConn >= 64){ 
				cout << "连接超过64，拒绝当前连接！" << endl;
				char buf[MSGSIZE] = "";
				strcpy(buf, "当前并发连接已经达到64个，不接受您的连接！");
				send(sockClient, buf, strlen(buf)+1, 0);
				closesocket(sockClient); 
			}
			else{
				cout << "接收该连接的请求，先将其放到数组中等待子线程处理" << endl;
				
				EnterCriticalSection(&ConnectLock);
				g_Client[g_nSockConn].addrClient = addrClient;	//保存客户端地址信息
				g_Client[g_nSockConn].sockClient = sockClient;	//加入连接者队列
				g_nSockConn++;
				LeaveCriticalSection(&ConnectLock);
			}
		}
	}

	cout << "关闭服务端socket" << endl;
	closesocket(sockListen);
	
	cout << "删除临界区锁" << endl;
	DeleteCriticalSection(&ConnectLock);

	cout << "调用WSACleanup" << endl;
	WSACleanup();

	system("pause");
	return 0;
}

/********************************************************************************************************
**************工作线程*************
工作线程是一个死循环，依次循环完成的动作是：
1.将当前客户端套接字假如fd_read集合中
2.调用select函数
3.用FD_ISSET查看是否套接字还在读集合中，如果是就接收数据
	如果接收的数据长度为0，或者发生WSAECONNRESET错误，则表示客户端套接字主动关闭，我们要释放这个套接字资源，调整我们的套接字数组（让下一个补上）
	上面还有0 == nRet 的判断，就是因为select函数会立即返回，连接数为0会陷入死循环！
*********************************************************************************************************/
DWORD WINAPI WorkThread(LPVOID lpParameter)
{
	FD_SET fdRead;		//声明一个集合
	int nRet = 0;		//记录发送或接收的字节数
	TIMEVAL tv;			//设置超时等待时间
	tv.tv_sec = 1;
	tv.tv_usec = 0;
	char buf[MSGSIZE] = "";

	while(true){
		//cout << "[子线程]循环开始将读集合清空" << endl;
		FD_ZERO(&fdRead);
		int i;

		//cout << "[子线程]将客户端数组中的所有socket添加到fdread" << endl;
		EnterCriticalSection(&ConnectLock);
		for(i=0; i<g_nSockConn; i++){
			FD_SET(g_Client[i].sockClient, &fdRead);
		}
		LeaveCriticalSection(&ConnectLock);		

		//只处理read事件，不过后面还是会有读写消息发送的
		//cout << "[子线程]调用select获取客户的socket" << endl;
		nRet = select(0, &fdRead, NULL, NULL, &tv);
	
		if(0 == nRet){
			//cout << "调用select无连接或读事件，Sleep(10)，进入下一轮循环" << endl;
			Sleep(10);

			//因为select函数会立即返回，连接数为0就会陷入死循环
			continue;
		}

		//cout << "[子线程]遍历g_Client数组，判断其是否在fdread集合中" << endl;
		EnterCriticalSection(&ConnectLock);
		i = 0;
		while(true){
			//如果数组当前socket的索引大于数组所有socket的个数，则退出内层循环
			if(i > g_nSockConn-1){
				break;
			}

			//select 返回后，它会修改每个fd_set结构，删除那些不存在待决IO的套接字，依然保留在集合中的就是本次可处理的socket
			//这正是我们在这里为什么要用FD_ISSET宏来判断一个特定的套接字是否仍在集合中的原因
			if(FD_ISSET(g_Client[i].sockClient, &fdRead)){

				cout << "[子线程]套接字" << g_Client[i].sockClient << "还在读集合中，将接收其数据" << endl;
				nRet = recv(g_Client[i].sockClient, buf, sizeof(buf), 0);

				//如果接收的数据长度是0，或发生WSAECONNERESET错误（说明客户端套接字主动关闭）
				//我们就要释放这个套接字资源，调整我们的套接字集合（让下一个补上）
				if(0 == nRet || (SOCKET_ERROR == nRet && WSAGetLastError() == WSAECONNRESET)){
					cout << "[子线程]关闭客户端" << inet_ntoa(g_Client[i].addrClient.sin_addr) << endl;
					closesocket(g_Client[i].sockClient);

					if (i < g_nSockConn-1){
						cout << "[子线程]将当前客户端socket删除，并调整g_Client数组" << endl;
						//用数组中后面的那个socket连接放到当前失效的socket所在的位置，移除当前失效的socket
						g_Client[i] = g_Client[g_nSockConn - 1];
						i--;
						//因为数组的最后的那个socket移到当前位置，所以将数组长度缩小
						g_nSockConn--;
					}
				}
				else{
					cout << "[子线程]收到客户端" << inet_ntoa(g_Client[i].addrClient.sin_addr) << "信息: " << buf << endl;

					strcpy(buf, "Hello!\n");
					cout << "[子线程]回复给客户端回复消息：" << buf << endl;
					nRet = send(g_Client[i].sockClient, buf, strlen(buf)+1, 0);
					cout << "[子线程]回复成功" << endl; 
				}
			}
			i++;
		}
		LeaveCriticalSection(&ConnectLock);	
	}

	return 0;
}