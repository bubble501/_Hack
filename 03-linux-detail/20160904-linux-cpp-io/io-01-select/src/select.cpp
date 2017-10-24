#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
	if(argc <= 2)
	{
		printf("usage: %s ip_address port_number\n", basename(argv[0]));
		return 1;
	}
	const char* ip = argv[1];
	int port = atoi(argv[2]);

	int ret = 0;
	struct sockaddr_in address;
	bzero(&address, sizeof(address));
	inet_pton(AF_INET, ip, &address.sin_addr);
	address.sin_port = htons(port);

	int listenfd = socket(PF_INET, SOCK_STREAM, 0);
	assert(listenfd >= 0);
	ret = bind(listenfd, (struct sockaddr* )&address, sizeof(address));
	assert(ret != -1);
	ret = listen(listenfd, 5);
	assert(ret != -1);

	struct sockaddr_in client_address;
	socklen_t client_addrlength = sizeof(client_address);
	int connfd = accept(listenfd, (struct sockaddr* )&client_address, &client_addrlength);
	if(connfd < 0)
	{
		printf("errno: %d\n", errno);
		close(listenfd);
	}

	char buf[1024];
	fd_set read_fds;
	fd_set exception_fds;
	FD_ZERO(&read_fds);			//清除read_fds上的所有位
	FD_ZERO(&exception_fds);

	while(1)
	{
		memset(buf, '\0', sizeof(buf));
		/*每次调用select前都要重新在read_fds和exception_fds中设置文件描述符connfd
		 * 因为每次事件发生后，文件描述符集合将会被内核修改*/
		FD_SET(connfd, &read_fds);		//设置read_fds的位connfd
		FD_SET(connfd, &exception_fds);

		/*int select(int nfds, fd_set* readfds, fd_set* writefds, fd_set* exceptionfds, struct timeval* timeout)
		 * nfds参数指定被监听的文件描述符的总数，它通常设置为select监听的所有文件描述符中的最大值加1，因为文件描述符是从0开始计数的
		 * readfds、writefds、exceptionfds参数分别指向可读、可写和异常事件对应的文件描述集合
		 *		应用程序调用select函数时，通过这3个参数传入自己感兴趣的文件描述符
		 *		select调用返回时，内核将修改它们来通知应用程序哪些文件描述符已经就绪
		 * timeout参数用来设置select函数的超时时间
		 *		不过不能完全信任select调用返回后的timeout值，比如调用失败时timeout值是不确定的
		 *		如果给timeout变量的tv_sec成员和tv_usec成员都传0，那么select将立即返回
		 *		如果给timeout传NULL，则select将一直阻塞，直到某个文件描述符就绪
		 *		所谓阻塞，就是程序运行到对应的这段代码行，就停止不继续运行了
		 * select成功时返回就绪(可读、可写和异常)文件描述符的总数
		 *		如果在超时时间内没有任何文件描述符就绪，select返回0
		 *		select失败时返回-1，并设置errno
		 *		如果在select等待期间，程序接收到信号，则select立即返回-1，并设置errno为EINTR*/
		ret = select(connfd + 1, &read_fds, NULL, &exception_fds, NULL);
		if(ret < 0)
		{
			printf("selection failure\n");
			break;
		}

		/*对于可读事件，采用普通的recv函数读取数据*/
		if(FD_ISSET(connfd, &read_fds))		//FD_ISSET测试read_fds的位connfd是否被设置
		{
			ret = recv(connfd, buf, sizeof(buf) - 1, 0);
			if(ret <= 0)
			{
				break;
			}
			printf("get %d bytes of normal data: %s\n", ret, buf);
		}
		/*对于异常事件，采用带MSG_OOB标志的recv函数读取带外数据*/
		else if(FD_ISSET(connfd, &exception_fds))
		{
			ret = recv(connfd, buf, sizeof(buf) - 1, MSG_OOB);
			if(ret <= 0)
			{
				break;
			}
			printf("get %d bytes of oob data: %s\n", ret, buf);
		}
	}
	close(connfd);
	close(listenfd);
	return 0;
}
