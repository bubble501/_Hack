#include<windows.h>	
#include<stdio.h>
#include<stdlib.h>

#define MAXLEN 1024

int main()
{
	printf("1-打开记事本\n");
	printf("2-关闭记事本\n");
	printf("3-修改记事本标题\n");
	printf("4-获取记事本标题\n");
	printf("其他-退出程序\n");
	
	int input;
	while(1){
		printf("请输入：");
		scanf("%d", &input);
		if (1 == input){
			//打开记事本
			WinExec("notepad.exe", SW_SHOW);
		}
		else if (2 == input){
			//关闭记事本
			HWND hWnd = FindWindow("Notepad", NULL);
			if (NULL == hWnd){
				printf("没有找到记事本！\n");
				break;
			}
			SendMessage(hWnd, WM_CLOSE, NULL, NULL);
		}
		else if (3 == input){
			//修改记事本标题
			HWND hWnd = FindWindow(NULL, "无标题 - 记事本");
			if (NULL == hWnd){
				printf("没有找到记事本！\n");
				break;
			}
			char *pCaption = "测试消息";
			SendMessage(hWnd, WM_SETTEXT, (WPARAM)0, (LPARAM)pCaption);	
		}
		else if (4 == input){
			//获取记事本标题
			HWND hWnd = FindWindow("Notepad", NULL);
			if (NULL == hWnd){
				printf("没有找到记事本！\n");
				break;
			}
			char pCaption[MAXLEN] = {0};
			SendMessage(hWnd, WM_GETTEXT, (WPARAM)MAXLEN, (LPARAM)pCaption);
			printf("记事本的标题是：%s\n", pCaption);
		}
		else{
			printf("退出程序\n");
			break;
		}
	}

	system("pause");
	return 0;
}