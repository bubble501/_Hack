#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

int main()
{
	pid_t child_pid;
	int i;

	/*创建一个子进程*/
	child_pid = fork();		//fork()创建一个进程

	//fork()系统调用创建一个新的进程，并且有两个返回值
	if (child_pid == 0)
	{
		//在子进程中的返回值是0
		for(i=0; i<100; i++)
		{
			fputc('c', stderr);
		}
	}
	else
	{
		//在主进程中的返回值是创建的子进程的ID
		for(i=0; i<100; i++)
		{
			fputc('f', stderr);
		}
	}
	return 0;
}
