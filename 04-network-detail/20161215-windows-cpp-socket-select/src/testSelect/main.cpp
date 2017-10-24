#include<WinSock2.h>
#include<stdio.h>
#include<stdlib.h>
#include<iostream>

using namespace std;

#pragma comment(lib, "ws2_32.lib")

#define PORT 8000
//#define MSGSIZE 1024 * 1024	�����������Ļ����ᵼ����������ʱջ��������ƺ��ڴ�ջ�ռ��һЩ�����й�
#define MSGSIZE 10240
#define SRV_IP "127.0.0.1"

int g_nSockConn = 0;			//�������ӵ���Ŀ
CRITICAL_SECTION ConnectLock;	//�ٽ����������ڱ�֤�̰߳�ȫ

struct ClientInfo{
	SOCKET sockClient;		//�ͻ����׽�����Ϣ
	SOCKADDR_IN addrClient;	//�ͻ��˵�ַ��Ϣ
};

//FD_SETSIZE����winsocket2.h�ж���ģ�����WindowsĬ�������64
//�ڰ���winsocket2.h֮ǰʹ�ú궨������޸����ֵ
//�洢�ͻ������ӵ����飬��Ϊ�ǲ��ԣ�����������Ϳ��Ը㶨���������������ö��е����ݽṹ��ʵ��
ClientInfo g_Client[FD_SETSIZE];	

DWORD WINAPI WorkThread(LPVOID lpParameter);

/********************************************************************************************************
*********����������Ҫ����**********
1.���������׽���-->��-->����
2.���������߳�
3.����һ���׽����飬������ŵ�ǰ���л�Ŀͻ����׽��֣�ÿacceptһ�����Ӿ͸���һ������
4.���տͻ��˵����ӣ���Ϊû�����¶���FD_SIZE�꣬���������֧��64���������ӣ�����Ǽ�¼������������Ҫ�������Ľ������ӣ�
********************************************************************************************************/
int main(int argc, char* argv[])
{
	cout << "����WSAStartup" << endl;
	WSADATA wsaData;
	WSAStartup(MAKEWORD(2, 2), &wsaData);

	cout << "��ʼ���ٽ�����" << endl;
	InitializeCriticalSection(&ConnectLock);

	cout << "���������socket" << endl;
	SOCKET sockListen = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

	SOCKADDR_IN addrSrv;
	addrSrv.sin_addr.S_un.S_addr = inet_addr(SRV_IP);
	addrSrv.sin_family = AF_INET;
	addrSrv.sin_port = htons(PORT);

	cout << "�����socket�󶨵�ַ���˿�" << endl;
	bind(sockListen, (SOCKADDR *)&addrSrv, sizeof(SOCKADDR));

	cout << "�����socket��ʼ����" << endl;
	listen(sockListen, 64);

	DWORD dwThreadIDRecv = 0;
	DWORD dwThreadIDWrite = 0;

	cout << "�������������շ���Ϣ���߳�" << endl;
	HANDLE hand = CreateThread(NULL, 0, &WorkThread, NULL, 0, &dwThreadIDRecv);
	if(NULL == hand){
		cout << "���������߳�ʧ�ܣ�\n";
		getchar();
		return -1;
	}

	SOCKET sockClient;
	SOCKADDR_IN addrClient;
	int nLenAddrClient = sizeof(addrClient);
	
	cout << "��ʼѭ��" << endl;
	while(true){
		//����������һ������addrClient�Ĵ�С��ʼ��
		sockClient = accept(sockListen, (SOCKADDR *)&addrClient, &nLenAddrClient);
		
		cout << "�յ�һ���ͻ��˵�����" << endl;
		if(INVALID_SOCKET != sockClient){
			cout << inet_ntoa(addrClient.sin_addr) << ": " << ntohs(addrClient.sin_port) << " ���ӳɹ���" << endl;

			if(g_nSockConn >= 64){ 
				cout << "���ӳ���64���ܾ���ǰ���ӣ�" << endl;
				char buf[MSGSIZE] = "";
				strcpy(buf, "��ǰ���������Ѿ��ﵽ64�����������������ӣ�");
				send(sockClient, buf, strlen(buf)+1, 0);
				closesocket(sockClient); 
			}
			else{
				cout << "���ո����ӵ������Ƚ���ŵ������еȴ����̴߳���" << endl;
				
				EnterCriticalSection(&ConnectLock);
				g_Client[g_nSockConn].addrClient = addrClient;	//����ͻ��˵�ַ��Ϣ
				g_Client[g_nSockConn].sockClient = sockClient;	//���������߶���
				g_nSockConn++;
				LeaveCriticalSection(&ConnectLock);
			}
		}
	}

	cout << "�رշ����socket" << endl;
	closesocket(sockListen);
	
	cout << "ɾ���ٽ�����" << endl;
	DeleteCriticalSection(&ConnectLock);

	cout << "����WSACleanup" << endl;
	WSACleanup();

	system("pause");
	return 0;
}

/********************************************************************************************************
**************�����߳�*************
�����߳���һ����ѭ��������ѭ����ɵĶ����ǣ�
1.����ǰ�ͻ����׽��ּ���fd_read������
2.����select����
3.��FD_ISSET�鿴�Ƿ��׽��ֻ��ڶ������У�����Ǿͽ�������
	������յ����ݳ���Ϊ0�����߷���WSAECONNRESET�������ʾ�ͻ����׽��������رգ�����Ҫ�ͷ�����׽�����Դ���������ǵ��׽������飨����һ�����ϣ�
	���滹��0 == nRet ���жϣ�������Ϊselect�������������أ�������Ϊ0��������ѭ����
*********************************************************************************************************/
DWORD WINAPI WorkThread(LPVOID lpParameter)
{
	FD_SET fdRead;		//����һ������
	int nRet = 0;		//��¼���ͻ���յ��ֽ���
	TIMEVAL tv;			//���ó�ʱ�ȴ�ʱ��
	tv.tv_sec = 1;
	tv.tv_usec = 0;
	char buf[MSGSIZE] = "";

	while(true){
		//cout << "[���߳�]ѭ����ʼ�����������" << endl;
		FD_ZERO(&fdRead);
		int i;

		//cout << "[���߳�]���ͻ��������е�����socket��ӵ�fdread" << endl;
		EnterCriticalSection(&ConnectLock);
		for(i=0; i<g_nSockConn; i++){
			FD_SET(g_Client[i].sockClient, &fdRead);
		}
		LeaveCriticalSection(&ConnectLock);		

		//ֻ����read�¼����������滹�ǻ��ж�д��Ϣ���͵�
		//cout << "[���߳�]����select��ȡ�ͻ���socket" << endl;
		nRet = select(0, &fdRead, NULL, NULL, &tv);
	
		if(0 == nRet){
			//cout << "����select�����ӻ���¼���Sleep(10)��������һ��ѭ��" << endl;
			Sleep(10);

			//��Ϊselect�������������أ�������Ϊ0�ͻ�������ѭ��
			continue;
		}

		//cout << "[���߳�]����g_Client���飬�ж����Ƿ���fdread������" << endl;
		EnterCriticalSection(&ConnectLock);
		i = 0;
		while(true){
			//������鵱ǰsocket������������������socket�ĸ��������˳��ڲ�ѭ��
			if(i > g_nSockConn-1){
				break;
			}

			//select ���غ������޸�ÿ��fd_set�ṹ��ɾ����Щ�����ڴ���IO���׽��֣���Ȼ�����ڼ����еľ��Ǳ��οɴ����socket
			//����������������ΪʲôҪ��FD_ISSET�����ж�һ���ض����׽����Ƿ����ڼ����е�ԭ��
			if(FD_ISSET(g_Client[i].sockClient, &fdRead)){

				cout << "[���߳�]�׽���" << g_Client[i].sockClient << "���ڶ������У�������������" << endl;
				nRet = recv(g_Client[i].sockClient, buf, sizeof(buf), 0);

				//������յ����ݳ�����0������WSAECONNERESET����˵���ͻ����׽��������رգ�
				//���Ǿ�Ҫ�ͷ�����׽�����Դ���������ǵ��׽��ּ��ϣ�����һ�����ϣ�
				if(0 == nRet || (SOCKET_ERROR == nRet && WSAGetLastError() == WSAECONNRESET)){
					cout << "[���߳�]�رտͻ���" << inet_ntoa(g_Client[i].addrClient.sin_addr) << endl;
					closesocket(g_Client[i].sockClient);

					if (i < g_nSockConn-1){
						cout << "[���߳�]����ǰ�ͻ���socketɾ����������g_Client����" << endl;
						//�������к�����Ǹ�socket���ӷŵ���ǰʧЧ��socket���ڵ�λ�ã��Ƴ���ǰʧЧ��socket
						g_Client[i] = g_Client[g_nSockConn - 1];
						i--;
						//��Ϊ����������Ǹ�socket�Ƶ���ǰλ�ã����Խ����鳤����С
						g_nSockConn--;
					}
				}
				else{
					cout << "[���߳�]�յ��ͻ���" << inet_ntoa(g_Client[i].addrClient.sin_addr) << "��Ϣ: " << buf << endl;

					strcpy(buf, "Hello!\n");
					cout << "[���߳�]�ظ����ͻ��˻ظ���Ϣ��" << buf << endl;
					nRet = send(g_Client[i].sockClient, buf, strlen(buf)+1, 0);
					cout << "[���߳�]�ظ��ɹ�" << endl; 
				}
			}
			i++;
		}
		LeaveCriticalSection(&ConnectLock);	
	}

	return 0;
}