#include<stdio.h>
#include<stdlib.h>
#include<windows.h>

#define STRLEN 20

/*
	该结构体中保存了LoaadLibraryA()、GetProcAddress()、GetModuleHandle()和GetModuleFileName()函数的地址
	这四个函数都属于Kernel32.dll，因此在所有进程中的地址都一直

	User32Dll中保存"User32.dll"字符串，因为MessageBoxA()函数是User32.dll的导出函数
	Str中保存的是通过MessageBoxA()函数弹出的字符串
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

	//定义API函数原型
	HMODULE (__stdcall *MyLoadLibrary)(LPCTSTR);
	FARPROC (__stdcall *MyGetProcAddress)(HMODULE, LPCSTR);		//typedef CONST CHAR *LPCSTR, *PCSTR;
	HMODULE (__stdcall *MyGetModuleHandle)(LPCTSTR);			//typedef LPCSTR LPCTSTR;
	int (__stdcall *MyMessageBox)(HWND, LPCTSTR, LPCTSTR, UINT);
	DWORD (__stdcall *MyGetModuleFileName)(HMODULE, LPTSTR, DWORD);

	//对个函数地址进行赋值
	MyLoadLibrary = (HMODULE (__stdcall *)(LPCTSTR))pData->dwLoadLibrary;
	MyGetProcAddress = (FARPROC (__stdcall *)(HMODULE, LPCSTR))pData->dwGetProcAddress;
	MyGetModuleHandle = (HMODULE (__stdcall *)(LPCSTR))pData->dwGetModuleHandle;
	MyGetModuleFileName = (DWORD (_stdcall *)(HMODULE, LPTSTR, DWORD))pData->dwGetModuleFileName;

	//远程线程在被注入进程内部加载User32.dll
	HMODULE hModule = MyLoadLibrary(pData->User32Dll);
	//远程线程在被注入进程内部获得MessageBoxA函数的地址
	MyMessageBox = (int (__stdcall *)(HWND, LPCTSTR, LPCTSTR, UINT))MyGetProcAddress(hModule, pData->MessageBox);

	char szModuleFileName[MAX_PATH] = { 0 };
	MyGetModuleFileName(NULL, szModuleFileName, MAX_PATH);

	MyMessageBox(NULL, pData->Str, szModuleFileName, MB_OK);

	return 0;
}

int main()
{
	/*
		调整当前进程权限，否则在调用创建远程线程的地方会导致被注入进程崩溃
		主要是调用WriteProcessMemory时无法将内容写到目标进程
		调试该问题花费2小时时间
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
		printf("调整进程权限成功\n");
	}
	else{
		system("pause");
		return 1;
	}


	DWORD dwPid;
	printf("请输入被注入的进程ID：");
	scanf("%d", &dwPid);
	//打开进程句柄
	HANDLE hProcess = OpenProcess(PROCESS_ALL_ACCESS, FALSE, dwPid);
	if(hProcess == NULL){
		system("pause");
		return 2;
	}

	DATA Data = { 0 };

	//获取kernel32.dll相关的导出函数
	Data.dwLoadLibrary = (DWORD)GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");
	Data.dwGetProcAddress = (DWORD)GetProcAddress(GetModuleHandle("kernel32.dll"), "GetProcAddress");
	Data.dwGetModuleHandle = (DWORD)GetProcAddress(GetModuleHandle("kernel32.dll"), "GetModuleHandleA");
	Data.dwGetModuleFileName = (DWORD)GetProcAddress(GetModuleHandle("kernel32.dll"), "GetModuleFileNameA");

	//需要的其他DLL和导出函数
	lstrcpy(Data.User32Dll, "user32.dll");
	lstrcpy(Data.MessageBox, "MessageBoxA");
	lstrcpy(Data.Str, "Hello World !");

	//在目标进程中申请空间
	printf("在目标进程中申请内存\n");
	/*
		在我的机器上后两个参数必须是MEM_COMMIT, PAGE_EXECUTE_READWRITE
		否则后面调用WriteProcessMemory，实际写到目标进程中的字节数是0
		该问题花费1小时时间调试解决！
	*/
	LPVOID lpData = VirtualAllocEx(hProcess, NULL, sizeof(Data), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	
	DWORD dwWriteNum = 0;
	printf("将Data拷贝到目标进程\n");
	WriteProcessMemory(hProcess, lpData, &Data, sizeof(Data), &dwWriteNum);
	printf("写到目标进程的字节数 = %d\n", dwWriteNum);
	
	//在目标进程空间申请的用于保存代码的长度
	DWORD dwFunSize = 0x400000;
	printf("在目标进程中申请内存用于保存线程函数\n");
	LPVOID lpCode = VirtualAllocEx(hProcess, NULL, dwFunSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);

	//将当前进程中的函数指令拷贝到目标进程，然后根据返回的目标进程中的线程函数地址创建远程线程！
	//不光内存中的数据可以拷贝，内存中的代码指令也是可以拷贝的
	printf("将线程函数拷贝到目标进程\n");
	WriteProcessMemory(hProcess, lpCode, &RemoteThreadProc, dwFunSize, &dwWriteNum);
	printf("写到目标进程的字节数 = %d\n", dwWriteNum);

	//创建远程线程
	printf("开始创建远程线程\n");
	//lpCode是远程线程函数在被注入进程的首地址
	//lpData是拷贝到目标进程的远程线程函数的参数
	HANDLE hThread = CreateRemoteThread(hProcess, NULL, 0, (LPTHREAD_START_ROUTINE)lpCode, lpData, 0, NULL);
	
	//测试代码：在该进程内部测试线程运行情况
	//HANDLE hThread = CreateThread(NULL, 0, RemoteThreadProc, &Data, 0, NULL);
	printf("远程线程创建完成\n");

	WaitForSingleObject(hThread, INFINITE);
	printf("WaitForSingleObject完成\n");

	CloseHandle(hThread);
	printf("CloseHandle(hThread)完成\n");
	CloseHandle(hProcess);
	printf("CloseHandle(hProcess)完成\n");

	system("pause");
	return 0;
}