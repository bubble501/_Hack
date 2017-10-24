#include<windows.h>		
#include<tlhelp32.h>
#include<stdio.h>
#include<stdlib.h>

int main()
{
	/*
		��ΪOpenThread��VC6Ĭ���ṩ��PSDK�в����ڣ�������Ҫ�����߶�̬����
	*/
	typedef HANDLE (WINAPI* PFN_OpenThread)(DWORD dwDesiredAccess, BOOL bInheritHandle, DWORD dwThreadId);
	HINSTANCE hDll = LoadLibrary("kernel32.dll");
	FARPROC pFnOnDll = GetProcAddress(hDll, "OpenThread");


	/*
		������ͣ�ͻָ�����/�߳�
	*/
	DWORD procId;
	printf("����Ҫ��ͣ�Ľ���ID��");
	scanf("%d", &procId);
	
	/*
		ö���̣߳��ҵ����ڸý��̵���������ͣ���߳�
	*/
	HANDLE hSnap = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, procId);
	if (hSnap == INVALID_HANDLE_VALUE){
		printf("CreateToolhelp32Snapshot Error! \n");
		system("pause");
		return 1;
	}
	THREADENTRY32 Te32 = { 0 };
	Te32.dwSize = sizeof(THREADENTRY32);
	BOOL bRet = Thread32First(hSnap, &Te32);
	//ѭ���߳̿����е�ÿһ��
	while (bRet) {
		if (Te32.th32OwnerProcessID == procId){
			//���߳�
			HANDLE hThread = ((PFN_OpenThread)pFnOnDll)(THREAD_ALL_ACCESS, FALSE, Te32.th32ThreadID);
			//��ͣ�߳�
			SuspendThread(hThread);
			CloseHandle(hThread);
		}
		bRet = Thread32Next(hSnap, &Te32);
	}
	CloseHandle(hSnap);

	int choice;
	printf("\n�Ƿ�ָ��̣߳�1-Yes/2-No��");
	scanf("%d", &choice);
	if (1 != choice){
		system("pause");
		return 2;
	}
	hSnap = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, procId);
	if (hSnap == INVALID_HANDLE_VALUE){
		printf("CreateToolhelp32Snapshot Error! \n");
		system("pause");
		return 3;
	}
	Te32.dwSize = sizeof(THREADENTRY32);
	bRet = Thread32First(hSnap, &Te32);
	//ѭ���߳̿����е�ÿһ��
	while (bRet) {
		if (Te32.th32OwnerProcessID == procId){
			//���߳�
			HANDLE hThread = ((PFN_OpenThread)pFnOnDll)(THREAD_ALL_ACCESS, FALSE, Te32.th32ThreadID);
			//��ͣ�߳�
			ResumeThread(hThread);
			CloseHandle(hThread);
		}
		bRet = Thread32Next(hSnap, &Te32);
	}
	CloseHandle(hSnap);

	system("pause");
	return 0;
}
