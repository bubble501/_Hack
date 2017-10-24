#include<stdio.h>
#include<stdlib.h>
#include<windows.h>

#define STRLEN 20

/*
	�ýṹ���б�����LoaadLibraryA()��GetProcAddress()��GetModuleHandle()��GetModuleFileName()�����ĵ�ַ
	���ĸ�����������Kernel32.dll����������н����еĵ�ַ��һֱ

	User32Dll�б���"User32.dll"�ַ�������ΪMessageBoxA()������User32.dll�ĵ�������
	Str�б������ͨ��MessageBoxA()�����������ַ���
*/
typedef struct _DATA{
	DWORD dwLoadLibrary;
	DWORD dwGetProcAddress;
	DWORD dwGetModuleHandle;
	DWORD dwGetModuleFileName;

	char User32Dll[STRLEN];
	char MessageBox[STRLEN];
	char Str[STRLEN];
}DATA, *PDATA;

DWORD WINAPI RemoteThreadProc(LPVOID lpParam)
{
	PDATA pData = (PDATA)lpParam;

	//����API����ԭ��
	HMODULE (__stdcall *MyLoadLibrary)(LPCTSTR);
	FARPROC (__stdcall *MyGetProcAddress)(HMODULE, LPCSTR);		//typedef CONST CHAR *LPCSTR, *PCSTR;
	HMODULE (__stdcall *MyGetModuleHandle)(LPCTSTR);			//typedef LPCSTR LPCTSTR;
	int (__stdcall *MyMessageBox)(HWND, LPCTSTR, LPCTSTR, UINT);
	DWORD (__stdcall *MyGetModuleFileName)(HMODULE, LPTSTR, DWORD);

	//�Ը�������ַ���и�ֵ
	MyLoadLibrary = (HMODULE (__stdcall *)(LPCTSTR))pData->dwLoadLibrary;
	MyGetProcAddress = (FARPROC (__stdcall *)(HMODULE, LPCSTR))pData->dwGetProcAddress;
	MyGetModuleHandle = (HMODULE (__stdcall *)(LPCSTR))pData->dwGetModuleHandle;
	MyGetModuleFileName = (DWORD (_stdcall *)(HMODULE, LPTSTR, DWORD))pData->dwGetModuleFileName;

	//Զ���߳��ڱ�ע������ڲ�����User32.dll
	HMODULE hModule = MyLoadLibrary(pData->User32Dll);
	//Զ���߳��ڱ�ע������ڲ����MessageBoxA�����ĵ�ַ
	MyMessageBox = (int (__stdcall *)(HWND, LPCTSTR, LPCTSTR, UINT))MyGetProcAddress(hModule, pData->MessageBox);

	char szModuleFileName[MAX_PATH] = { 0 };
	MyGetModuleFileName(NULL, szModuleFileName, MAX_PATH);

	MyMessageBox(NULL, pData->Str, szModuleFileName, MB_OK);

	return 0;
}

int main()
{
	/*
		������ǰ����Ȩ�ޣ������ڵ��ô���Զ���̵߳ĵط��ᵼ�±�ע����̱���
		��Ҫ�ǵ���WriteProcessMemoryʱ�޷�������д��Ŀ�����
		���Ը����⻨��2Сʱʱ��
	*/
	HANDLE hToken = NULL;
	BOOL bRet = OpenProcessToken(GetCurrentProcess(), TOKEN_ALL_ACCESS, &hToken);
	if (bRet == TRUE){
		TOKEN_PRIVILEGES tp;
		tp.PrivilegeCount = 1;
		LookupPrivilegeValue(NULL, SE_DEBUG_NAME, &tp.Privileges[0].Luid);
		tp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
		AdjustTokenPrivileges(hToken, FALSE, &tp, sizeof(tp), NULL, NULL);
		
		CloseHandle(hToken);
		printf("��������Ȩ�޳ɹ�\n");
	}
	else{
		system("pause");
		return 1;
	}


	DWORD dwPid;
	printf("�����뱻ע��Ľ���ID��");
	scanf("%d", &dwPid);
	//�򿪽��̾��
	HANDLE hProcess = OpenProcess(PROCESS_ALL_ACCESS, FALSE, dwPid);
	if(hProcess == NULL){
		system("pause");
		return 2;
	}

	DATA Data = { 0 };

	//��ȡkernel32.dll��صĵ�������
	Data.dwLoadLibrary = (DWORD)GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");
	Data.dwGetProcAddress = (DWORD)GetProcAddress(GetModuleHandle("kernel32.dll"), "GetProcAddress");
	Data.dwGetModuleHandle = (DWORD)GetProcAddress(GetModuleHandle("kernel32.dll"), "GetModuleHandleA");
	Data.dwGetModuleFileName = (DWORD)GetProcAddress(GetModuleHandle("kernel32.dll"), "GetModuleFileNameA");

	//��Ҫ������DLL�͵�������
	lstrcpy(Data.User32Dll, "user32.dll");
	lstrcpy(Data.MessageBox, "MessageBoxA");
	lstrcpy(Data.Str, "Hello World !");

	//��Ŀ�����������ռ�
	printf("��Ŀ������������ڴ�\n");
	/*
		���ҵĻ����Ϻ���������������MEM_COMMIT, PAGE_EXECUTE_READWRITE
		����������WriteProcessMemory��ʵ��д��Ŀ������е��ֽ�����0
		�����⻨��1Сʱʱ����Խ����
	*/
	LPVOID lpData = VirtualAllocEx(hProcess, NULL, sizeof(Data), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	
	DWORD dwWriteNum = 0;
	printf("��Data������Ŀ�����\n");
	WriteProcessMemory(hProcess, lpData, &Data, sizeof(Data), &dwWriteNum);
	printf("д��Ŀ����̵��ֽ��� = %d\n", dwWriteNum);
	
	//��Ŀ����̿ռ���������ڱ������ĳ���
	DWORD dwFunSize = 0x400000;
	printf("��Ŀ������������ڴ����ڱ����̺߳���\n");
	LPVOID lpCode = VirtualAllocEx(hProcess, NULL, dwFunSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);

	//����ǰ�����еĺ���ָ�����Ŀ����̣�Ȼ����ݷ��ص�Ŀ������е��̺߳�����ַ����Զ���̣߳�
	//�����ڴ��е����ݿ��Կ������ڴ��еĴ���ָ��Ҳ�ǿ��Կ�����
	printf("���̺߳���������Ŀ�����\n");
	WriteProcessMemory(hProcess, lpCode, &RemoteThreadProc, dwFunSize, &dwWriteNum);
	printf("д��Ŀ����̵��ֽ��� = %d\n", dwWriteNum);

	//����Զ���߳�
	printf("��ʼ����Զ���߳�\n");
	//lpCode��Զ���̺߳����ڱ�ע����̵��׵�ַ
	//lpData�ǿ�����Ŀ����̵�Զ���̺߳����Ĳ���
	HANDLE hThread = CreateRemoteThread(hProcess, NULL, 0, (LPTHREAD_START_ROUTINE)lpCode, lpData, 0, NULL);
	
	//���Դ��룺�ڸý����ڲ������߳��������
	//HANDLE hThread = CreateThread(NULL, 0, RemoteThreadProc, &Data, 0, NULL);
	printf("Զ���̴߳������\n");

	WaitForSingleObject(hThread, INFINITE);
	printf("WaitForSingleObject���\n");

	CloseHandle(hThread);
	printf("CloseHandle(hThread)���\n");
	CloseHandle(hProcess);
	printf("CloseHandle(hProcess)���\n");

	system("pause");
	return 0;
}