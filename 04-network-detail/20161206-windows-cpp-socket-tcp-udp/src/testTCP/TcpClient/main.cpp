#include<stdio.h>
#include<stdlib.h>
#include<winsock2.h>
#pragma comment (lib, "ws2_32")

int main()
{
	WSADATA wsaData;
	WSAStartup(MAKEWORD(2, 2), &wsaData);

	//�����׽���
	SOCKET sServer = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

	//��sockaddr_in �ṹ������ַ���˿���Ϣ
	struct sockaddr_in ServerAddr;
	ServerAddr.sin_family  =AF_INET;
	ServerAddr.sin_addr.S_un.S_addr = inet_addr("127.0.0.1");
	ServerAddr.sin_port = htons(1234);

	//���ӷ�����
	connect(sServer, (SOCKADDR *)&ServerAddr, sizeof(ServerAddr));

	char szMsg[MAXBYTE] = { 0 };

	//������Ϣ
	recv(sServer, szMsg, MAXBYTE, 0);
	printf("Server Msg : %s \r\n", szMsg);

	//������Ϣ
	lstrcpy(szMsg, "hello Server! \r\n");
	send(sServer, szMsg, strlen(szMsg) + sizeof(char), 0);

	WSACleanup();

	system("pause");
	return 0;
}