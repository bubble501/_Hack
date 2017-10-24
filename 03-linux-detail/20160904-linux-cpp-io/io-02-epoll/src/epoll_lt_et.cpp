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
#include <sys/epoll.h>
#include <pthread.h>

#define MAX_EVENT_NUMBER 1024
#define BUFFER_SIZE 10

/*将文件描述符设置程非阻塞的*/
int setnonblocking(int fd)
{
	int old_option = fcntl(fd, F_GETFL);
	int new_option = old_option | O_NONBLOCK;
	fcntl(fd, F_SETFL, new_option);
	return old_option;
}

/*将文件描述符fd上的EPOLLIN注册到epollfd指示的epoll内核事件表中
 * 参数enable_et指定是否对fd启用ET模式*/
void addfd(int epollfd, int fd, bool enable_et)
{
	epoll_event event;
	event.data.fd = fd;
	event.events = EPOLLIN;
	if(enable_et)
	{
		event.events |= EPOLLET;
	}
	/*int epoll_ctl(int epfd, int op, int fd, struct epoll_event* event)用于操作epoll内核事件表
	 * op参数指定操作类型，包括往事件表注册时间、修改注册事件、删除注册事件
	 * fd参数是要操作的文件描述符
	 * event参数指定事件*/
	epoll_ctl(epollfd, EPOLL_CTL_ADD, fd, &event);
	
	/*将文件描述符设置成非阻塞的*/
	setnonblocking(fd);
}

/*LT模式的工作流程*/
void lt(epoll_event* events, int number, int epollfd, int listenfd)
{
	char buf[BUFFER_SIZE];
	for(int i=0; i<number; i++)
	{
		int sockfd = events[i].data.fd;
		if(listenfd == sockfd)
		{
			struct sockaddr_in client_address;
			socklen_t client_addrlength = sizeof(client_address);
			int connfd = accept(listenfd, (struct sockaddr* )&client_address, &client_addrlength);
			addfd(epollfd, connfd, false);	//对connfd禁用ET模式
		}
		else if(events[i].events & EPOLLIN)
		{
			/*只要socket读缓存中还有未读出的数据，这段代码就被触发*/
			printf("event trigger once\n");
			memset(buf, '\0', BUFFER_SIZE);
			int ret = recv(sockfd, buf, BUFFER_SIZE - 1, 0);
			if(ret <= 0)
			{
				close(sockfd);
				continue;
			}
			printf("get %d bytes of content: %s\n", ret, buf);
		}
		else
		{
			printf("something else happened\n");
		}
	}
}

/*ET模式的工作流程*/
void et(epoll_event* events, int number, int epollfd, int listenfd)
{
	char buf[BUFFER_SIZE];
	for(int i=0; i< number; i++)
	{
		int sockfd = events[i].data.fd;
		if(sockfd == listenfd)
		{
			struct sockaddr_in client_address;
			socklen_t client_addrlength = sizeof(client_address);
			int connfd = accept(listenfd, (struct sockaddr* )&client_address, &client_addrlength);
			addfd(epollfd, connfd, true);	//对connfd开启ET模式
		}
		else if(events[i].events & EPOLLIN)
		{
			/*这段代码不会被重复触发，所以我们循环读取数据，以确保把socket读缓存中的所有数据读出*/
			printf("event trigger once\n");
			while(1)
			{
				memset(buf, '\0', BUFFER_SIZE);
				int ret = recv(sockfd, buf, BUFFER_SIZE - 1, 0);
				if(ret < 0)
				{
					/*对于非阻塞IO，下面的条件成立表示数据已经完全读取完毕
					 * 此后，epoll就再次触发sockfd上的EPOLLIN事件，以驱动下一次读操作*/
					if((errno == EAGAIN) || (errno == EWOULDBLOCK))
					{
						printf("read later\n");
						break;
					}
					close(sockfd);
					break;
				}
				else if(ret == 0)
				{
					close(sockfd);
				}
				else
				{
					printf("get %d bytes of content: %s\n", ret, buf);
				}
			}
		}
		else
		{
			printf("something else happened\n");
		}
	}
}


int main(int argc, char* argv[])
{
	if(argc <= 2)
	{
		printf("usage: %s ip_address, port_number\n", basename(argv[0]));
		return 1;
	}
	const char* ip = argv[1];
	int port = atoi(argv[2]);

	int ret = 0;
	struct sockaddr_in address;
	bzero(&address, sizeof(address));
	address.sin_family = AF_INET;
	inet_pton(AF_INET, ip, &address.sin_addr);
	address.sin_port = htons(port);

	int listenfd = socket(PF_INET, SOCK_STREAM, 0);
	assert(listenfd >= 0);

	ret = bind(listenfd, (struct sockaddr* )&address, sizeof(address));
	assert(ret != -1);

	ret = listen(listenfd, 5);
	assert(ret != -1);

	epoll_event events[MAX_EVENT_NUMBER];
	/*epoll需要使用一个额外的文件描述符，来唯一标识内核中的这个事件表
	 * 这个文件描述符使用如下epoll_create(int size)函数来创建
	 * size参数现在并不起作用，只是给内核一个提示，告诉他事件表需要多大
	 * 该函数返回一个文件描述符将用作其他所有epoll系统调用的第一个参数，以指定要访问的内核事件表*/
	int epollfd = epoll_create(5);
	assert(epollfd != -1);

	addfd(epollfd, listenfd, true);
	
	while(1)
	{
		/*epoll系列系统调用的主要接口是epoll_wait函数
		 * 它在一段超时时间内等待一组文件描述符上的事件
		 * 该函数成功时返回就绪的文件描述符的格式，失败时返回-1并设置errno
		 * epoll_wait函数如果检测到事件，就将所有就绪的事件从内核时间表(由epfd参数指定)中复制到它的第二个参数events指向的数组中
		 * 这个数组只用与输出epoll_wait检测到的就绪事件
		 * 而不像select和poll的数组参数那样既用于传入用户注册的事件，又用于输出内核检测到的就绪事件
		 * 这就极大地提高了应用程序索引就绪文件描述符的效率*/
		int ret = epoll_wait(epollfd, events, MAX_EVENT_NUMBER, -1);
		if(ret < 0)
		{
			printf("epoll failure\n");
			break;
		}
		
		//lt(events, ret, epollfd, listenfd);			//使用LT模式
		et(events, ret, epollfd, listenfd);		//使用ET模式
	}
	close(listenfd);
	return 0;
}
