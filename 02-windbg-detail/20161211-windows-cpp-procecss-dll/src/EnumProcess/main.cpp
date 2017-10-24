#include<windows.h>		
#include<tlhelp32.h>	//必须先#include<windows.h>，否则#include<tlhelp32.h>报错
#include<stdio.h>
#include<stdlib.h>


int main()
{
	/*
		枚举系统进程信息
	*/
	//创建进程快照
	HANDLE hSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
	if (hSnap == INVALID_HANDLE_VALUE){
		printf("CreateToolhelp32Snapshot Error!\n");
		system("pause");
		return 1;
	}

	PROCESSENTRY32 Pe32 = { 0 };
	Pe32.dwSize = sizeof(PROCESSENTRY32);	//使用该结构体时必须对该结构体的dwSize变量赋值。保存PROCESSENTRY32结构体大小

	//枚举函数。获取第一个进程的信息
	BOOL bRet = Process32First(hSnap, &Pe32);
	int i = 0;
	//循环获取进程快照中的每一项
	while (bRet){
		//输出进程信息，当然PROCESSENTRY32结构体中不止进程名和进程ID信息
		printf("进程名：%-30s | 进程ID：%7d \n", Pe32.szExeFile, Pe32.th32ProcessID);

		bRet = Process32Next(hSnap, &Pe32);
	}
	CloseHandle(hSnap);

	/*
		将当前进程的权限提升为SeDebugPrivilege，否则接下来调用CreateToolhelp32Snapshot创建模块快照失败
	*/
	HANDLE hToken = NULL;
	//打开当前进程的访问令牌
	BOOL bTokenRet = OpenProcessToken(GetCurrentProcess(), TOKEN_ALL_ACCESS, &hToken);
	if (bTokenRet == TRUE){
		TOKEN_PRIVILEGES tp;
		tp.PrivilegeCount = 1;
		//获取描述权限的LUID
		LookupPrivilegeValue(NULL, SE_DEBUG_NAME, &tp.Privileges[0].Luid);
		tp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
		//调整访问令牌的权限
		AdjustTokenPrivileges(hToken, FALSE, &tp, sizeof(tp), NULL, NULL);

		CloseHandle(hToken);
	}

	/*
		选择某进程，枚举进程的DLL信息
	*/
	DWORD proId;
	printf("请输入进程ID，查看该进程加载的DLL信息！进程ID：");
	scanf("%d", &proId);
	MODULEENTRY32 Me32 = { 0 };
	Me32.dwSize = sizeof(MODULEENTRY32);
	//创建模块快照
	HANDLE hModuleSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, proId);
	if (hModuleSnap == INVALID_HANDLE_VALUE){
		printf("CreateToolhelp32Snapshot Error!\n");
		system("pause");
		return 2;
	}
	BOOL bModuleRet = Module32First(hModuleSnap, &Me32);
	while (bModuleRet) {
		//输出DLL相关信息
		printf("DLL名：%s \n", Me32.szExePath);

		bModuleRet = Module32Next(hModuleSnap, &Me32);
	}
	CloseHandle(hModuleSnap);

	system("pause");
	return 0;
}