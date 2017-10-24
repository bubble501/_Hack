#include<windows.h>		
#include<tlhelp32.h>	//������#include<windows.h>������#include<tlhelp32.h>����
#include<stdio.h>
#include<stdlib.h>


int main()
{
	/*
		ö��ϵͳ������Ϣ
	*/
	//�������̿���
	HANDLE hSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
	if (hSnap == INVALID_HANDLE_VALUE){
		printf("CreateToolhelp32Snapshot Error!\n");
		system("pause");
		return 1;
	}

	PROCESSENTRY32 Pe32 = { 0 };
	Pe32.dwSize = sizeof(PROCESSENTRY32);	//ʹ�øýṹ��ʱ����Ըýṹ���dwSize������ֵ������PROCESSENTRY32�ṹ���С

	//ö�ٺ�������ȡ��һ�����̵���Ϣ
	BOOL bRet = Process32First(hSnap, &Pe32);
	int i = 0;
	//ѭ����ȡ���̿����е�ÿһ��
	while (bRet){
		//���������Ϣ����ȻPROCESSENTRY32�ṹ���в�ֹ�������ͽ���ID��Ϣ
		printf("��������%-30s | ����ID��%7d \n", Pe32.szExeFile, Pe32.th32ProcessID);

		bRet = Process32Next(hSnap, &Pe32);
	}
	CloseHandle(hSnap);

	/*
		����ǰ���̵�Ȩ������ΪSeDebugPrivilege���������������CreateToolhelp32Snapshot����ģ�����ʧ��
	*/
	HANDLE hToken = NULL;
	//�򿪵�ǰ���̵ķ�������
	BOOL bTokenRet = OpenProcessToken(GetCurrentProcess(), TOKEN_ALL_ACCESS, &hToken);
	if (bTokenRet == TRUE){
		TOKEN_PRIVILEGES tp;
		tp.PrivilegeCount = 1;
		//��ȡ����Ȩ�޵�LUID
		LookupPrivilegeValue(NULL, SE_DEBUG_NAME, &tp.Privileges[0].Luid);
		tp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
		//�����������Ƶ�Ȩ��
		AdjustTokenPrivileges(hToken, FALSE, &tp, sizeof(tp), NULL, NULL);

		CloseHandle(hToken);
	}

	/*
		ѡ��ĳ���̣�ö�ٽ��̵�DLL��Ϣ
	*/
	DWORD proId;
	printf("���������ID���鿴�ý��̼��ص�DLL��Ϣ������ID��");
	scanf("%d", &proId);
	MODULEENTRY32 Me32 = { 0 };
	Me32.dwSize = sizeof(MODULEENTRY32);
	//����ģ�����
	HANDLE hModuleSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, proId);
	if (hModuleSnap == INVALID_HANDLE_VALUE){
		printf("CreateToolhelp32Snapshot Error!\n");
		system("pause");
		return 2;
	}
	BOOL bModuleRet = Module32First(hModuleSnap, &Me32);
	while (bModuleRet) {
		//���DLL�����Ϣ
		printf("DLL����%s \n", Me32.szExePath);

		bModuleRet = Module32Next(hModuleSnap, &Me32);
	}
	CloseHandle(hModuleSnap);

	system("pause");
	return 0;
}