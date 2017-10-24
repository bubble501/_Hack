#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
 int sockfd;
 int len;
 struct sockaddr_in address;
 int result;
 char ch = 'A';

 //创建客户端socket
 sockfd = socket(AF_INET, SOCK_STREAM, 0);
 address.sin_family = AF_INET;
 //inet_addr()用于将十进制的IP转换成长整型数
 address.sin_addr.s_addr = inet_addr("127.0.0.1");
 //将整型变量从主机字节序转变成网络字节序
 address.sin_port = htons(9734);
 len = sizeof(address);
 //向服务端发起连接
 result = connect(sockfd, (struct sockaddr *)&address, len);

 //如果连接失败
 if (result == -1)
 {
  perror("oops: client1");
  exit(1);
 }
 //如果连接成功：发送字符'A'
 write(sockfd, &ch, 1);
 //同步接收服务端应答
 read(sockfd, &ch, 1);
 //输出应答
 printf("char from server = %c\n", ch);
 //关闭socket
 close(sockfd);
 exit(0);
}
