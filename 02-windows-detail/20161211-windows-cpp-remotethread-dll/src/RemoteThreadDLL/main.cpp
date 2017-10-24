#include<windows.h>
#include<tlhelp32.h>
#include<stdio.h>
#include<stdlib.h>

#define PATHLEN 1024

int main()
{
	DWORD procId;
	printf("�����뱻ע�����ID��");
	scanf("%d", &procId);

	char DllPath[PATHLEN];
	printf("������DLL·����");
	scanf("%s", DllPath);

	//��Ŀ�����
	HANDLE hProcess = OpenProcess(PROCESS_ALL_ACCESS, FALSE, procId);
	if(NULL == hProcess){
		printf("OpenProcess Error!\n");
		system("pause");
		return 1;
	}

	//������ע��DLL�ļ�����·���ĳ��ȣ���Ҫ��������'\0'
	int nDllLen = lstrlen(DllPath) + sizeof(char);

	/*
		��Ŀ�����������һ�鳤��ΪnDllLen���ڴ�ռ�
		������Ŀ����������뵽���ڴ����ʼ��ַ
		����1��Ŀ����̾��
		����2��ָ����Ŀ������������ڴ����ʼ��ַ������ΪNULL����ʾ��ָ��
		����3����Ŀ�������������ڴ��С
		����4���ò���ָ�������ڴ��״̬����
		����5��ָ�������ڴ������
	*/
	PVOID pDllAddr = VirtualAllocEx(hProcess, NULL, nDllLen, MEM_COMMIT, PAGE_READWRITE);
	if(NULL == pDllAddr){
		printf("VirtualAllocEx Error!\n");
		CloseHandle(hProcess);
		system("pause");
		return 2;
	}

	DWORD dwWriteNum = 0;

	/*
		����ע��DLL�ļ�������·��д�뵽Ŀ�������������ڴ�ռ�
		����1��ָ�����̵ľ��
		����2��ָ��д��Ŀ������ڴ����ʼ��ַ
		����3��Ҫд��Ŀ������ڴ�Ļ�������ʼ��ַ��λ�ڱ����̣�
		����4��ָ��д��Ŀ���ڴ��еĻ���������
		����5������ʵ��д�����ݵĳ���
	*/
	WriteProcessMemory(hProcess, pDllAddr, DllPath, nDllLen, &dwWriteNum);

	/*
		��ȡLoadLibraryA()�����ĵ�ַ
		��ΪLoadLibraryA()��ϵͳ��Kernel32.dll�ĵ���������Kernel32.dll���DLL���κν����еļ��ص�ַ����ͬ
		����ֻҪ�ڵ�ǰ�����л�ȡLoadLibraryA()�ĺ�����ַ����ӦҲ������Ŀ������еĵ�ַ
	*/
	char *pFunName = "LoadLibraryA";
	FARPROC pFuncAddr = GetProcAddress(GetModuleHandle("kernel32.dll"), pFunName);

	/*
		����Զ���߳�
		����1��Ŀ����̵ľ��
		...
		����4��ָ���̺߳����ĵ�ַ
		����5�����ݸ��̺߳����Ĳ���
		...

		��Ϊ�̺߳����Ķ����LoadLibraryA()�Ķ�����˺�������ֵ���ͺͲ�������֮�⣬�亯����ʽ��ͬ������ֻ��������ͬ����
		��Ϊ������ʽ��ͬ�����ȵ���Լ����ͬ������WINAPI(Ҳ����__stdcall��ʽ��
		��κ���������ͬ��ֻ��һ��
		����ֱ�ӿ��Խ�LoadLibraryA()��Ϊ�̺߳�������
	*/
	HANDLE hThread = CreateRemoteThread(hProcess, NULL, 0, 
										(LPTHREAD_START_ROUTINE)pFuncAddr, pDllAddr, 0, NULL);

	WaitForSingleObject(hThread, INFINITE);
	CloseHandle(hThread);
	CloseHandle(hProcess);

	/*
		ж��DLL
	*/
	int choice;
	printf("�Ƿ�ж��DLL��1-Yes/2-No��");
	scanf("%d", &choice);
	if (choice != 1){
		printf("δ����1����ж��DLL\n");
		system("pause");
		return 3;
	}

	//��ö��Ŀ����̵�DLL
	HANDLE hSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, procId);
	MODULEENTRY32 me32;
	me32.dwSize = sizeof(MODULEENTRY32);

	//����ƥ��Ľ�������
	BOOL bRet = Module32First(hSnap, &me32);
	while(bRet){
		if (lstrcmp(strupr(me32.szExePath), strupr(DllPath)) == 0){
			break;
		}
		bRet = Module32Next(hSnap, &me32);
	}
	CloseHandle(hSnap);

	//��ȡFreeLibrary��ַ��FreeLibrary(DllHandle)
	char *FreeFunName = "FreeLibrary";
	
	//�򿪽���
	hProcess = OpenProcess(PROCESS_ALL_ACCESS, FALSE, procId);
	if(NULL == hProcess){
		printf("OpenProcess Error!\n");
		system("pause");
		return 4;
	}

	FARPROC FreeAddr = GetProcAddress(GetModuleHandle("kernel32.dll"), FreeFunName);

	hThread = CreateRemoteThread(hProcess, NULL, 0, 
								(LPTHREAD_START_ROUTINE)FreeAddr, me32.hModule, 0, NULL);

	WaitForSingleObject(hThread, INFINITE);
	CloseHandle(hThread);
	CloseHandle(hProcess);

	system("pause");
	return 0;
}