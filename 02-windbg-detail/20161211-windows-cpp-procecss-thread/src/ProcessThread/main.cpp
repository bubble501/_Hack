#include<windows.h>		
#include<tlhelp32.h>
#include<stdio.h>
#include<stdlib.h>

int main()
{
	/*
		因为OpenThread在VC6默认提供的PSDK中不存在，所以需要开发者动态加载
	*/
	typedef HANDLE (WINAPI* PFN_OpenThread)(DWORD dwDesiredAccess, BOOL bInheritHandle, DWORD dwThreadId);
	HINSTANCE hDll = LoadLibrary("kernel32.dll");
	FARPROC pFnOnDll = GetProcAddress(hDll, "OpenThread");


	/*
		测试暂停和恢复进程/线程
	*/
	DWORD procId;
	printf("输入要暂停的进程ID：");
	scanf("%d", &procId);
	
	/*
		枚举线程，找到属于该进程的现在则暂停该线程
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
	//循环线程快照中的每一项
	while (bRet) {
		if (Te32.th32OwnerProcessID == procId){
			//打开线程
			HANDLE hThread = ((PFN_OpenThread)pFnOnDll)(THREAD_ALL_ACCESS, FALSE, Te32.th32ThreadID);
			//暂停线程
			SuspendThread(hThread);
			CloseHandle(hThread);
		}
		bRet = Thread32Next(hSnap, &Te32);
	}
	CloseHandle(hSnap);

	int choice;
	printf("\n是否恢复线程，1-Yes/2-No：");
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
	//循环线程快照中的每一项
	while (bRet) {
		if (Te32.th32OwnerProcessID == procId){
			//打开线程
			HANDLE hThread = ((PFN_OpenThread)pFnOnDll)(THREAD_ALL_ACCESS, FALSE, Te32.th32ThreadID);
			//暂停线程
			ResumeThread(hThread);
			CloseHandle(hThread);
		}
		bRet = Thread32Next(hSnap, &Te32);
	}
	CloseHandle(hSnap);

	system("pause");
	return 0;
}
