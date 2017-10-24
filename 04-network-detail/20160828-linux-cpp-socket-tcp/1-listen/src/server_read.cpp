#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

static bool stop = false;
/*SIGTERM信号的处理函数，触发时结束主程序中的循环*/
static void handle_term(int sig)
{
	stop = true;
}

int main(int argc, char* argv[])
{
	//signal函数的作用是为指定的信号安装一个新的信号处理函数，第一个参数是指定信号，第二个是对应的函数指针
	signal(SIGTERM, handle_term);	

	if (argc <= 3)
	{
		//basename的作用是得到特定路径中的最后一个'/'后的内容，比如/usr/bin，得到的内容就是bin
		printf("usage: %s ip_address port_number backlog\n", basename(argv[0]));
		return 1;
	}
	const char* ip = argv[1];
	int port = atoi(argv[2]);					//atoi()用于将字符串转换成对应整数
	int backlog = atoi(argv[3]);

	/* PF_INET用于IPv4; SOCK_STREAM表示传输层使用TCP协议
	 * socket()调用成功则返回一个socket描述符，失败则返回-1，并设置errno
	 */
	int sock = socket(PF_INET, SOCK_STREAM, 0);
	assert(sock >= 0);

	/*创建一个IPv4 socket地址*/
	struct sockaddr_in address;
	bzero(&address, sizeof(address));			//bzero(void* s, int n)，用于将s指向的地址以及连续后n个地址的字节清零
	address.sin_family = AF_INET;
	inet_pton(AF_INET, ip, &address.sin_addr);	//inet_pton函数将字符串表示的IP地址转换成网络字节序整数表示的IP地址，既可用于IPv4又可用于IPv6
	address.sin_port = htons(port);				//htons函数将短整型的主机字节序数据转为网络字节序数据

	/*将socket绑定到对应的IP、端口上*/
	int ret = bind(sock, (struct sockaddr* )&address, sizeof(address));
	assert(ret != -1);

	ret = listen(sock, backlog);
	assert(ret != -1);

	/*循环等待连接，直到有SIGTERM信号将它中断*/
	while(!stop)
	{
		sleep(1);
	}

	/*关闭socket*/
	close(sock);
	return 0;
}
