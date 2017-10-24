#include<stdio.h>
#include<stdlib.h>
#include<winsock2.h>
#pragma comment (lib, "ws2_32")

int main()
{
	WSADATA wsaData;
	/* 第一个参数是需要初始化Winsock库的版本号，目前常用的是2.2
	 * 第二个参数是一个纸箱WSADATA的指针
	 * 返回0，说明调用成功，返回其他值调用失败
	 */
	WSAStartup(MAKEWORD(2, 2), &wsaData);

	//创建套接字
	SOCKET sLisent = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

	//对sockaddr_in结构体填充地址、端口等信息
	struct sockaddr_in ServerAddr;
	ServerAddr.sin_family = AF_INET;
	ServerAddr.sin_addr.S_un.S_addr = inet_addr("127.0.0.1");	//inet_addr将IP字符串转成in_addr结构体可接受的类型
	ServerAddr.sin_port = htons(1234);							//htons()将主机字节序转换成网络字节序
	
	//绑定套接字与地址信息
	bind(sLisent, (SOCKADDR *)&ServerAddr, sizeof(ServerAddr));

	//监听端口
	listen(sLisent, SOMAXCONN);

	//获取连接请求
	sockaddr_in ClientAddr;
	int nSize = sizeof(ClientAddr);

	SOCKET sClient = accept(sLisent, (SOCKADDR *)&ClientAddr, &nSize);

	//输出客户端使用的IP和端口号
	printf("ClientIAddr=%s:%d\r\n", inet_ntoa(ClientAddr.sin_addr),		//将IP地址转换成字符串格式
		ntohs(ClientAddr.sin_port));									//ntohs()将网络字节序转成主机字节序

	//发送消息
	char szMsg[MAXBYTE] = { 0 };
	lstrcpy(szMsg, "hello Client!\r\n");
	send(sClient, szMsg, strlen(szMsg) + sizeof(char), 0);

	//接收消息
	recv(sClient, szMsg, MAXBYTE, 0);
	printf("Client Msg : %s \r\n", szMsg);

	//
	WSACleanup();

	system("pause");
	return 0;
}