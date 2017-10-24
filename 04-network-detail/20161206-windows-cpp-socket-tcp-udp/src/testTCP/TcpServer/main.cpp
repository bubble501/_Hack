#include<stdio.h>
#include<stdlib.h>
#include<winsock2.h>
#pragma comment (lib, "ws2_32")

int main()
{
	WSADATA wsaData;
	/* ��һ����������Ҫ��ʼ��Winsock��İ汾�ţ�Ŀǰ���õ���2.2
	 * �ڶ���������һ��ֽ��WSADATA��ָ��
	 * ����0��˵�����óɹ�����������ֵ����ʧ��
	 */
	WSAStartup(MAKEWORD(2, 2), &wsaData);

	//�����׽���
	SOCKET sLisent = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

	//��sockaddr_in�ṹ������ַ���˿ڵ���Ϣ
	struct sockaddr_in ServerAddr;
	ServerAddr.sin_family = AF_INET;
	ServerAddr.sin_addr.S_un.S_addr = inet_addr("127.0.0.1");	//inet_addr��IP�ַ���ת��in_addr�ṹ��ɽ��ܵ�����
	ServerAddr.sin_port = htons(1234);							//htons()�������ֽ���ת���������ֽ���
	
	//���׽������ַ��Ϣ
	bind(sLisent, (SOCKADDR *)&ServerAddr, sizeof(ServerAddr));

	//�����˿�
	listen(sLisent, SOMAXCONN);

	//��ȡ��������
	sockaddr_in ClientAddr;
	int nSize = sizeof(ClientAddr);

	SOCKET sClient = accept(sLisent, (SOCKADDR *)&ClientAddr, &nSize);

	//����ͻ���ʹ�õ�IP�Ͷ˿ں�
	printf("ClientIAddr=%s:%d\r\n", inet_ntoa(ClientAddr.sin_addr),		//��IP��ַת�����ַ�����ʽ
		ntohs(ClientAddr.sin_port));									//ntohs()�������ֽ���ת�������ֽ���

	//������Ϣ
	char szMsg[MAXBYTE] = { 0 };
	lstrcpy(szMsg, "hello Client!\r\n");
	send(sClient, szMsg, strlen(szMsg) + sizeof(char), 0);

	//������Ϣ
	recv(sClient, szMsg, MAXBYTE, 0);
	printf("Client Msg : %s \r\n", szMsg);

	//
	WSACleanup();

	system("pause");
	return 0;
}