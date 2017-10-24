#include<iostream>
#include<cstdio>
#include<string>
#include<cstring>
#include<winsock2.h>
#include<Windows.h>

using namespace std;

#pragma comment(lib, "Ws2_32.lib")		//Socket编程需用的动态链接库

SOCKET sockClient;				//连接成功后的套接字
HANDLE bufferMutex;				//令其能互斥成功正常通信的信号量句柄
const int DefaultPort = 6000;

int main()
{
// 加载Socket动态链接库（dll）
	WORD wVersionRequested;  
	WSADATA wsaData;    // 这结构是用于接收Wjndows Socket的结构信息的   
	wVersionRequested = MAKEWORD( 2, 2 );   // 请求2.2版本的WinSock库   
	int err = WSAStartup( wVersionRequested, &wsaData );  
	if ( err != 0 ) {   // 返回值为零的时候是表示成功申请WSAStartup   
		return -1;  
	} 
	if ( LOBYTE( wsaData.wVersion ) != 2 || HIBYTE( wsaData.wVersion ) != 2 ) { // 检查版本号是否正确   
		WSACleanup( );  
		return -1;   
	}

//创建socket操作，建立流式套接字，返回套接字号sockClient
	sockClient = socket(AF_INET, SOCK_STREAM, 0);
	if(sockClient == INVALID_SOCKET) {   
		printf("Error at socket():%ld\n", WSAGetLastError());   
		WSACleanup();   
		return -1;   
	}  

//将套接字sockClient与远程主机连接
	SOCKADDR_IN addrSrv;
	addrSrv.sin_addr.S_un.S_addr = inet_addr("127.0.0.1");		//本地回路地址是127.0.0.1
	addrSrv.sin_family = AF_INET;
	addrSrv.sin_port = htons(DefaultPort);
	while(SOCKET_ERROR == connect(sockClient, (SOCKADDR*)&addrSrv, sizeof(SOCKADDR))){
		//如果没有连接上服务器则要求重连
		cout << "服务器连接失败，是否重新连接？（Y/N）：";
		char choice;
		while(cin >> choice && (!((choice!='Y' && choice=='N') || (choice=='Y' && choice!='N')))){
			cout << "输出错误，请重新输入：";
			cin.sync();
			cin.clear();
		}
		if(choice == 'Y'){
			continue;
		}
		else{
			cout << "退出系统中...";
			system ("pause");
			return 0;
		}
	}
	cin.sync();
	cout << "本客户端已准备就绪，用户可直接输入文字向服务器反馈信息。\n";

	send(sockClient, "n\Attention: A Client has enter...\n", 200, 0);
	bufferMutex = CreateSemaphore(NULL, 1, 1, NULL);

	DWORD WINAPI SendMessageThread(LPVOID lpParamter);
	DWORD WINAPI ReceiveMessageThread(LPVOID lpParameter);

	HANDLE sendThread = CreateThread(NULL, 0, SendMessageThread, NULL, 0, NULL);
	HANDLE receiveThread = CreateThread(NULL, 0, ReceiveMessageThread, NULL, 0, NULL);

	WaitForSingleObject(sendThread, INFINITE);	//等待线程结束
	closesocket(sockClient);
	CloseHandle(sendThread);
	CloseHandle(receiveThread);
	CloseHandle(bufferMutex);
	WSACleanup();   // 终止对套接字库的使用   

	printf("End linking...\n");  
	printf("\n");  
	system("pause");  
	return 0;  
}

DWORD WINAPI SendMessageThread(LPVOID lpParameter)
{
	while(1){
		string talk;
		getline(cin, talk);
		WaitForSingleObject(bufferMutex, INFINITE);		//P（资源未被占用）
		if("quit" == talk){
			talk.append("\0"); 
			send(sockClient, talk.c_str(), 200, 0);
			break;
		}
		else{
			talk.append("\n");
		}
		printf("\nI Say:(\"quit\" to exit):");
		cout << talk;
		send(sockClient, talk.c_str(), 200, 0);
		ReleaseSemaphore(bufferMutex, 1, NULL);			//V（资源占用完毕）
	}
	return 0;
}

DWORD WINAPI ReceiveMessageThread(LPVOID lpParameter)
{
	while(1){
		char recvBuf[300];
		recv(sockClient, recvBuf, 200, 0);
		WaitForSingleObject(bufferMutex, INFINITE);		//P（资源未被占用）
		
		printf("5s Says: %s", "Server", recvBuf);

		ReleaseSemaphore(bufferMutex, 1, NULL);			////V（资源占用完毕）
	}
}

