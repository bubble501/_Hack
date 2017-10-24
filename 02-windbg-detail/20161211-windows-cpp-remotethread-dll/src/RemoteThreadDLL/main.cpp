#include<windows.h>
#include<tlhelp32.h>
#include<stdio.h>
#include<stdlib.h>

#define PATHLEN 1024

int main()
{
	DWORD procId;
	printf("请输入被注入进程ID：");
	scanf("%d", &procId);

	char DllPath[PATHLEN];
	printf("请输入DLL路径：");
	scanf("%s", DllPath);

	//打开目标进程
	HANDLE hProcess = OpenProcess(PROCESS_ALL_ACCESS, FALSE, procId);
	if(NULL == hProcess){
		printf("OpenProcess Error!\n");
		system("pause");
		return 1;
	}

	//计算欲注入DLL文件完整路径的长度，不要忘记最后的'\0'
	int nDllLen = lstrlen(DllPath) + sizeof(char);

	/*
		在目标进程中申请一块长度为nDllLen的内存空间
		返回在目标进程中申请到的内存的起始地址
		参数1：目标进程句柄
		参数2：指定在目标进程中申请内存的起始地址，可以为NULL，表示不指定
		参数3：在目标进程中申请的内存大小
		参数4：该参数指定申请内存的状态类型
		参数5：指定申请内存的属性
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
		将欲注入DLL文件的完整路径写入到目标进程中申请的内存空间
		参数1：指定进程的句柄
		参数2：指定写入目标进程内存的起始地址
		参数3：要写入目标进程内存的缓冲区起始地址（位于本进程）
		参数4：指定写入目标内存中的缓冲区长度
		参数5：接收实际写入内容的长度
	*/
	WriteProcessMemory(hProcess, pDllAddr, DllPath, nDllLen, &dwWriteNum);

	/*
		获取LoadLibraryA()函数的地址
		因为LoadLibraryA()是系统中Kernel32.dll的导出函数，Kernel32.dll这个DLL在任何进程中的加载地址都相同
		所以只要在当前进程中获取LoadLibraryA()的函数地址，对应也就是在目标进程中的地址
	*/
	char *pFunName = "LoadLibraryA";
	FARPROC pFuncAddr = GetProcAddress(GetModuleHandle("kernel32.dll"), pFunName);

	/*
		创建远程线程
		参数1：目标进程的句柄
		...
		参数4：指定线程函数的地址
		参数5：传递给线程函数的参数
		...

		因为线程函数的定义和LoadLibraryA()的定义除了函数返回值类型和参数类型之外，其函数格式相同，这里只考虑其相同部分
		因为函数格式相同，首先调用约定相同，都是WINAPI(也就是__stdcall方式）
		其次函数个数相同，只有一个
		所以直接可以将LoadLibraryA()作为线程函数调用
	*/
	HANDLE hThread = CreateRemoteThread(hProcess, NULL, 0, 
										(LPTHREAD_START_ROUTINE)pFuncAddr, pDllAddr, 0, NULL);

	WaitForSingleObject(hThread, INFINITE);
	CloseHandle(hThread);
	CloseHandle(hProcess);

	/*
		卸载DLL
	*/
	int choice;
	printf("是否卸载DLL，1-Yes/2-No：");
	scanf("%d", &choice);
	if (choice != 1){
		printf("未输入1，不卸载DLL\n");
		system("pause");
		return 3;
	}

	//先枚举目标进程的DLL
	HANDLE hSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, procId);
	MODULEENTRY32 me32;
	me32.dwSize = sizeof(MODULEENTRY32);

	//查找匹配的进程名称
	BOOL bRet = Module32First(hSnap, &me32);
	while(bRet){
		if (lstrcmp(strupr(me32.szExePath), strupr(DllPath)) == 0){
			break;
		}
		bRet = Module32Next(hSnap, &me32);
	}
	CloseHandle(hSnap);

	//获取FreeLibrary地址，FreeLibrary(DllHandle)
	char *FreeFunName = "FreeLibrary";
	
	//打开进程
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