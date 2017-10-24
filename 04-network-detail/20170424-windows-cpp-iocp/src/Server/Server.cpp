#include <WinSock2.h>
#include <Windows.h>
#include <vector>
#include <iostream>

using namespace std;

#pragma comment(lib, "Ws2_32.lib")		//Socket�����Ҫʹ�õĶ�̬���ӿ�
#pragma comment(lib, "Kernel32.lib")	//IOCP��Ҫ�õ��Ķ�̬���ӿ�

/*
* �ṹ�����ƣ�PER_IO_DATA���ص��ṹ
* �ṹ�幦�ܣ��ص�IO��Ҫ�õ��Ľṹ�壬��ʱ��¼IO����
*/
const int DataBuffSize = 2 * 1024;
typedef struct
{
	/*typedef struct _OVERLAPPED { 
	����DWORD Internal;		//Ԥ����OSʹ�ã�ָ��һ��������ϵͳ��״̬����GetOverlappedResult��������ʱû��������չ������ϢERROR_IO_PENDINGʱ��Ч��
	����DWORD InternalHigh; //Ԥ����OSʹ�ã�ָ�����ȵ�����ת�ƣ���GetOverlappedResult��������TRUEʱ��Ч
	����DWORD Offset;		//���ļ���λ���Ǵ��ļ���ʼ�����ֽ�ƫ���������ý������������Ա֮ǰ����ReadFile��WriteFile����������ȡ��д�������ܵ���ͨ���豸ʱ�����Ա��������Ϊ��
	����DWORD OffsetHigh;	//ָ���ļ����͵��ֽ�ƫ�����ĸ�λ�֡�����ȡ��д�������ܵ���ͨ���豸ʱ�����Ա��������Ϊ��
	����HANDLE hEvent;		//��ת�����ʱ����һ���¼�����Ϊ���ź�״̬�����ý��̼������Ա�ڵ���ReadFile�� WriteFile��TransactNamedPipe�� ConnectNamedPipe����֮ǰ
	����} OVERLAPPED
	*/
	OVERLAPPED overlapped;
	/*��������WSASocket���ݵĻ���
	typedef struct __WSABUF
	{
		u_long len;
		char FAR *buf;
	}WSABUF, *LPWSABUF;
	�϶������������ɶ˿ڣ���ϵͳ���ں��л�ȡWSASocket����
	��������len�Ĵ�С����������Ҫ���len���ȵ�����
	����ʹ����ɶ˿ڵ�GetQueuedCompletionStatus��ȡ����
	*/
	WSABUF databuff;
	char buffer[DataBuffSize];	//const int DataBuffSize = 2 * 1024;
	int BufferLen;				//�����С
	int operationType;			//��־����ص�I/O��������ʲô�ģ�����Accept/Recv��  
}PER_IO_OPERATION_DATA, *LPPER_IO_OPERATION_DATA, *LPPER_IO_DATA, PER_IO_DATA;

/*
* �ṹ�����ƣ�PER_HANDLE_DATA
* �ṹ��洢����¼�����׽��ֵ����ݣ��������׽��ֵı������׽��ֵĶ�Ӧ�Ŀͻ��˵�ַ
* �ṹ�����ã��������������Ͽͻ���ʱ����Ϣ�洢���ýṹ���У�֪���ͻ��˵ĵ�ַ�Ա��ڻط�
*/
typedef struct
{
	SOCKET socket;				//socket
	SOCKADDR_IN ClientAddr;		//�ͻ��˵ĵ�ַ
}PER_HANDLE_DATA, *LPPER_HANDLE_DATA;

//����ȫ�ֱ���
const int DefaultPort = 6000;				//����˼����Ķ˿ں�
vector < PER_HANDLE_DATA* > clientGroup;	//��¼�ͻ��˵�������

HANDLE hMutex = CreateMutex(NULL, false, NULL);			//�������
DWORD WINAPI ServerWorkThread(LPVOID CompletionPortID);	//����˹����߳�
DWORD WINAPI ServerSendThread(LPVOID ipPrarm);			//����˷����߳�

//��ʼ������
int main()
{
	//����Socket��̬���ӿ�
	WORD wVersionRequested = MAKEWORD(2, 2);	//����2.2�汾��WinSock��
	WSADATA wsaData;							//����Windows Socket�Ľṹ��Ϣ
	DWORD err = WSAStartup(wVersionRequested, &wsaData);

	//����׽��ֿ��Ƿ�����ɹ�
	if(0 != err){
		cerr << "Request Windows Socket Library Error!\n";
		system("pause");
		return -1;
	}
	//����Ƿ�����������汾���׽��ֿ�
	if(LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2){
		WSACleanup();
		cerr << "Request Windows Socket Version 2.2 Error!\n";
		system("pause");
		return -1;
	}

//����IOCP���ں˺���
	/* ��Ҫ�õ��ĺ���ԭ�ͣ�
	 * HANDLE WINAPI CreateIoCompletionPort(
	 *		_in HANDLE FileHandle,				//�Ѿ��򿪵��ļ������վ����һ���ǿͻ��˵ľ��
	 *		_in HANDLE ExistingCompletionPort,	//�Ѿ����ڵ�IOCP���
	 *		_in ULONG_PTR CompletionKey,		//��ɼ���������ָ��IO��ɰ�ָ����ļ�
	 *		_in DWORD NumberOfConcurrentThreads	//��������ͬʱִ������߳�����һ���ƽ���CPU������*2
	 *	);
	 * ��һ�ε���CreateIoCompletionPort���ڴ���һ��IOCP���
	 * ��������CreateIoCompletionPort���ڽ�socket��IOCP������й���
	 */
	HANDLE completionPort = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
	if(NULL == completionPort){//����IO�ں˶���ʧ��
		cerr << "CreateIoCompletionPort Failed. Error: " << GetLastError() << endl;
		system("pause");
		return -1;
	}

//����IOCP�̣߳��߳����洴���̳߳�
	//ȷ���������ĺ�������
	SYSTEM_INFO mySysInfo;
	GetSystemInfo(&mySysInfo);

	//���ڴ������ĺ������������̣߳��߳����Ƽ�Ϊ��������2����
	for(DWORD i=0; i<(mySysInfo.dwNumberOfProcessors*2); i++){
		//�����������������̣߳�������ɶ˿ڴ��ݵ����߳�
		HANDLE ThreadHandle = CreateThread(NULL, 0, ServerWorkThread, completionPort, 0, NULL);
		if(NULL == ThreadHandle){
			cerr << "Create Thread Handle failed. Error:" << GetLastError() << endl;  
			system("pause");  
			return -1;  
		}
		CloseHandle(ThreadHandle);
	}

//������ʽ�׽���
	SOCKET srvSocket = socket(AF_INET, SOCK_STREAM, 0);

//��SOCKET������
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

//��SOCKET����Ϊ����ģʽ
	int listenResult = listen(srvSocket, 10);
	if(SOCKET_ERROR == listenResult){
		cerr << "Listen failed. Error: " << GetLastError() << endl;
		system("pause");
		return -1;
	}

//��ʼ����IO����
	 cout << "����������׼�����������ڵȴ��ͻ��˵Ľ���...\n";
	 
	 //�������ڷ������ݵ��߳�
	 HANDLE sendThread = CreateThread(NULL, 0, ServerSendThread, 0, 0, NULL);

	 //���߳��ڲ�ѭ��accept�ͻ�������
	 //���ҵ���CreateIoCompletionPort��socket��IOCP���������ϵ
	 while(true){
		/*  typedef struct
			{
				SOCKET socket;				//socket
				SOCKADDR_IN ClientAddr;		//�ͻ��˵ĵ�ַ
			}PER_HANDLE_DATA, *LPPER_HANDLE_DATA;			 
		�ýṹ�����ڱ���ͻ������ӹ�����socket�������Ϣ		*/
		PER_HANDLE_DATA * PerHandleData = NULL;
		SOCKADDR_IN saRemote;
		int RemoteLen;
		SOCKET acceptSocket;

		//�������ӣ���������ɶˣ����������AcceptEx()
		RemoteLen = sizeof(saRemote);
		acceptSocket = accept(srvSocket, (SOCKADDR*)&saRemote, &RemoteLen);
		if(SOCKET_ERROR == acceptSocket){//���տͻ���ʧ��
			cerr << "Accept Socket Error: " << GetLastError() << endl;
			system("pause");
			return -1;
		}

		//�����������׽��ֹ����ĵ����������Ϣ�ṹ
		PerHandleData = (LPPER_HANDLE_DATA)GlobalAlloc(GPTR, sizeof(PER_HANDLE_DATA));	//�ڶ���Ϊ���PerHandleData�����ö���С���ڴ�
		PerHandleData->socket = acceptSocket;						//�ͻ���socket
		memcpy(&PerHandleData->ClientAddr, &saRemote, RemoteLen);	//�ͻ��˵�ַ��Ϣ
		
		//�������ͻ�������ָ��ŵ��ͻ�������
		clientGroup.push_back(PerHandleData);		

		//�յ�һ���µ����Ӻ󣬽�������ӵ��׽��ֺ���ɶ˿ڹ���
		CreateIoCompletionPort((HANDLE)(PerHandleData->socket), completionPort, (DWORD)PerHandleData, 0);

		//��ʼ�ڽ����׽����ϴ���IOʹ���ص�IO����
		//���½����׽�����Ͷ��һ�������첽WSARecv��WSASend������ЩIO������ɺ󣬹����̻߳�ΪIO�����ṩ����

		//��IO�������ݣ�IO�ص���
		/*	typedef struct
			{
				OVERLAPPED overlapped;
				WSABUF databuff;
				char buffer[DataBuffSize];	//const int DataBuffSize = 2 * 1024;
				int BufferLen;				//�����С
				int operationType;			//��־����ص�I/O��������ʲô�ģ�����Accept/Recv��  
			}PER_IO_OPERATION_DATA, *LPPER_IO_OPERATION_DATA, *LPPER_IO_DATA, PER_IO_DATA;			*/
		LPPER_IO_OPERATION_DATA PerIoData = NULL;
		//�����ڴ�
		PerIoData = (LPPER_IO_OPERATION_DATA)GlobalAlloc(GPTR, sizeof(LPPER_IO_OPERATION_DATA));
		//���ڴ�����
		ZeroMemory(&(PerIoData->overlapped), sizeof(OVERLAPPED));
		//���ý��ջ���������Ϣ����С��ָ�룩
		PerIoData->databuff.len = 1024;
		PerIoData->databuff.buf = PerIoData->buffer;
		PerIoData->operationType = 0;		//read

		DWORD RecvBytes;
		DWORD Flags = 0;
		/* ���ص�ģ���У��������ݾ�Ҫ�����ˣ����Ĳ�����recvҪ�࣬��ΪҪ�õ��ص��ṹ
		 * ����1��ҪͶ�ݵ�����������׽���
		 * ����2����Ҫһ��WSABuff�Ľṹ������
		 * ����3��WSABuff�ṹ������Ԫ�ص�����
		 * ����4����ɽ��ղ���������ɣ�����᷵�غ������������յ����ֽ���
		 * ����5��lpFlags���������������κ�һ��ֵ��MSG_PEEK��MSG_OOB��MSG_PARTIAL���߶���Щֵ���а�λ������֮��Ľ��
		 * ����6��
		 * ���½����׽�����Ͷ��һ�������첽WSARecv��WSASend������ЩIO������ɺ󣬹����̻߳�ΪIO�����ṩ����
		*/
		WSARecv(PerHandleData->socket, &(PerIoData->databuff), 1, &RecvBytes, &Flags, &(PerIoData->overlapped), NULL);
	}

	system("pause");
	return 0;
}
 
/* ����IOCPģ����Ҫ�ر�˵��һ��
   * accept�߳�
     * һ��ר�ŵ��߳�����accept�ͻ�������
     * ��accept�߳��У���socket����ɶ˿ڽ��й���
     * ��accept�߳��У�����WSARecv�������ڸ��µ��׽�����Ͷ��һ���µ�WSARecv����Ҳ������д������
     * accept�̵߳���Ҫ��������������ѭ��
   * �����߳�
     * ����GetQueuedCompletionStatus���������ȴ�ĳ��socket��IO����¼������߳�ʱ
	 * GetQueuedCompletionStatus��ɺ󣬻��IO��ɵ�socket
	 * �����Ӧ�Ļ������е��Ѿ���ȡ�õ�����
	 * Ȼ���ٵ���WSARecv�ڸ�socket����Ͷ��һ��IO���󣬵ȴ�IO���
	 * ���ѭ��
 * select��IOCP�ĺܴ��������
   * IOCP����GetQueuedCompletionStatus���һ��socket���ں����Ѿ�����˶Ը�socket��IO�����������Ѿ����ں˽����������ݿ�����ָ���Ļ���������
   * ����Ҫ�ǹ��ڶ�IO��������д�Ļ��������������û�̬�������з������ݣ�Ȼ�����WSASendͶ��һ��д����
     * Ȼ���ں��з��ַ��ͻ������п�д��ʱ��ͻ������д�����ջ�������
	 * д����ɺ������ͨ��GetQueuedCompletionStatus��ȡ���¼�
   * selectģʽ�����ڵ���select�󣬻�ȡ�˿ɶ�����д��socket��Ȼ����Ҫ�Լ���ȥ����send��recv����ȥ���ж�д
   * IOCP�������IO֮��֪ͨ�㣬��ֱ��ȥ����IO�õ�����
   * select�ǿ��Խ���IO��ʱ��֪ͨ�㣬����ȥ�Լ�����IO��������IO��Ȼ����ȥ����IO�õ�����

* ��Щ�ǹ���IOCP��select�ļ򵥽���
   * ʵ�ʵ�Ӧ���У�����Ҫ�������ģʽ��ʹ��
   * ����Ҫ�����ں˷��ͻ��������˵����
   * ����Ҫ�����ں˽��ջ����������ݲ��㵼�¶�ȡ�����ݲ���ɵ����
   * �ȵ����������������Ҫ�ٽ��п��ǵ�
*/

//��ʼ�������̺߳���
DWORD WINAPI ServerWorkThread(LPVOID lpParam)
{
	HANDLE CompletionPort = (HANDLE)lpParam;
	DWORD BytesTransferred;
	LPOVERLAPPED lpOverlapped;
	/*	typedef struct
		{
			SOCKET socket;				//socket
			SOCKADDR_IN ClientAddr;		//�ͻ��˵ĵ�ַ
		}PER_HANDLE_DATA, *LPPER_HANDLE_DATA;	*/
	LPPER_HANDLE_DATA PerHandleData = NULL;
	/*	typedef struct
		{
			OVERLAPPED overlapped;
			WSABUF databuff;
			char buffer[DataBuffSize];	//const int DataBuffSize = 2 * 1024;
			int BufferLen;				//�����С
			int operationType;			//��־����ص�I/O��������ʲô�ģ�����Accept/Recv��  
		}PER_IO_OPERATION_DATA, *LPPER_IO_OPERATION_DATA, *LPPER_IO_DATA, PER_IO_DATA;			*/
	LPPER_IO_DATA PerIoData = NULL;
	DWORD RecvBytes;
	DWORD Flags = 0;
	BOOL bRet = false;

	while(true){
		/* GetQueuedCompletionStatusʹ�����̹߳���
		 * ֱ��ָ���Ķ˿ڵ�IO��ɶ����г���һ���ֱ����ʱ

		 * GetQueuedCompletionStatus�����ں���ɶ�дIO֮�����ص��ṹָ�����ڴ������д������
		 * ���ú���������һ��Socket֮��IO�����Ѿ�����ˣ�������ֱ�Ӷ�IO��ɵ����ݽ��д���ͺ��ˣ�����Ҫ�ٽ���IO������
		 * select��֪ͨ����Խ���IO�ˣ�Ȼ���������IO����
		 * IOCP���������IO֮�󣬸���������ˣ���ֱ��ȥ�������ݾͺ���
		 * �������������Ҫ
		 * ���滹Ӧ���жϵ����ǳɹ���ȡһ��Socket���ǵȴ���ʱ��
		*/
		bRet = GetQueuedCompletionStatus(CompletionPort, &BytesTransferred, (unsigned long*)&PerHandleData, (LPOVERLAPPED*)&lpOverlapped, INFINITE); 
		if(0 == bRet){
			cerr << "GetQueuedCompletionStatus Error: " << GetLastError() << endl;  
			return -1; 
		}

		/* GetQueuedCompletionStatusִ�гɹ���
		 * ͨ��GetQueuedCompletionStatus�ĵ�4���������Ի�ö�Ӧ��IO�ص��ṹPerIoData
		 * ����ʹ��PerIoData��ȡ��ȡ��������
		 */
		PerIoData = (LPPER_IO_DATA)CONTAINING_RECORD(lpOverlapped, PER_IO_DATA, overlapped);

		//����׽������Ƿ��д�����
		if(0 == BytesTransferred){
			closesocket(PerHandleData->socket);
			GlobalFree(PerHandleData);
			GlobalFree(PerIoData);
			continue;
		}

		//��ʼ���ݴ����������Կͻ��˵�����
		WaitForSingleObject(hMutex, INFINITE);
		//ͨ��PerIoData��ȡ������������Ϣ��
		cout << "A Client says: " << PerIoData->databuff.buf << endl;
		ReleaseMutex(hMutex);

		//Ϊ��һ���ص����ý�����IO��������
		ZeroMemory(&(PerIoData->overlapped), sizeof(OVERLAPPED));	//����ڴ�
		PerIoData->databuff.len = 1024;
		PerIoData->databuff.buf = PerIoData->buffer;
		PerIoData->operationType = 0;		//read

		//��������׽�����Ͷ��һ�������첽WSARecv��WSASend����
		//��ЩIO������ɺ󣬹����̻߳��ں�����ѭ������ΪIO�����ṩ����
		WSARecv(PerHandleData->socket, &(PerIoData->databuff), 1, &RecvBytes, &Flags, &(PerIoData->overlapped), NULL);
	}
	return 0;
}

//������Ϣ���߳�ִ�к���
DWORD WINAPI ServerSendThread(LPVOID lpParam)
{
	while(1){
		char talk[200];
		gets(talk);
		int len;
		for(len=0; talk[len]!='\0'; ++len){
			//�������ַ���ĳ���
		}
		talk[len] = '\n';
		talk[++len] = '\0';
		printf("I Say: ");
		cout << talk;

		WaitForSingleObject(hMutex, INFINITE);
		for(int i=0; i<clientGroup.size(); ++i){
			send(clientGroup[i]->socket, talk, 200, 0);		//������Ϣ
		}
		ReleaseMutex(hMutex);
	}
	return 0;
}
