#include<windows.h>	
#include<stdio.h>
#include<stdlib.h>

#define MAXLEN 1024

int main()
{
	printf("1-�򿪼��±�\n");
	printf("2-�رռ��±�\n");
	printf("3-�޸ļ��±�����\n");
	printf("4-��ȡ���±�����\n");
	printf("����-�˳�����\n");
	
	int input;
	while(1){
		printf("�����룺");
		scanf("%d", &input);
		if (1 == input){
			//�򿪼��±�
			WinExec("notepad.exe", SW_SHOW);
		}
		else if (2 == input){
			//�رռ��±�
			HWND hWnd = FindWindow("Notepad", NULL);
			if (NULL == hWnd){
				printf("û���ҵ����±���\n");
				break;
			}
			SendMessage(hWnd, WM_CLOSE, NULL, NULL);
		}
		else if (3 == input){
			//�޸ļ��±�����
			HWND hWnd = FindWindow(NULL, "�ޱ��� - ���±�");
			if (NULL == hWnd){
				printf("û���ҵ����±���\n");
				break;
			}
			char *pCaption = "������Ϣ";
			SendMessage(hWnd, WM_SETTEXT, (WPARAM)0, (LPARAM)pCaption);	
		}
		else if (4 == input){
			//��ȡ���±�����
			HWND hWnd = FindWindow("Notepad", NULL);
			if (NULL == hWnd){
				printf("û���ҵ����±���\n");
				break;
			}
			char pCaption[MAXLEN] = {0};
			SendMessage(hWnd, WM_GETTEXT, (WPARAM)MAXLEN, (LPARAM)pCaption);
			printf("���±��ı����ǣ�%s\n", pCaption);
		}
		else{
			printf("�˳�����\n");
			break;
		}
	}

	system("pause");
	return 0;
}