/* J. David's webserver */
/* This is a simple webserver.
 * Created November 1999 by J. David Blackstone.
 * CSE 4344 (Network concepts), Prof. Zeigler
 * University of Texas at Arlington
 */
/* This program compiles for Sparc Solaris 2.6.
 * To compile for Linux:
 *  1) Comment out the #include <pthread.h> line.
 *  2) Comment out the line that defines the variable newthread.
 *  3) Comment out the two lines that run pthread_create().
 *  4) Uncomment the line that runs accept_request().
 *  5) Remove -lsocket from the Makefile.
 */
#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <ctype.h>
#include <strings.h>
#include <string.h>
#include <sys/stat.h>
#include <pthread.h>
#include <sys/wait.h>
#include <stdlib.h>

//isspace()判断参数是否为空格字符' '、定位字符'\t'、等
#define ISspace(x) isspace((int)(x))
//定义服务端的版本信息
#define SERVER_STRING "Server: jdbhttpd/0.1.0\r\n"

//接受连接请求
void accept_request(int);
//通知客户端，HTTP请求导致了一个错误
void bad_request(int);
//将文件FILE*中数据发送给客户端
void cat(int, FILE *);
//通知客户端，CGI脚本无法执行
void cannot_execute(int);
//服务端出错时，退出程序，并输出错误信息
void error_die(const char *);
//执行CGI程序，返回输出给客户端
void execute_cgi(int, const char *, const char *, const char *);
//诸行获取客户端HTTP请求头字符串
int get_line(int, char *, int);
//打包HTML文件对应的HTTP头信息
void headers(int, const char *);
//客户端请求URL，在服务端未找到对应资源，返回404错误给客户端
void not_found(int);
//处理文件：打包HTTP报文头、读取文件内容发送给客户端
void serve_file(int, const char *);
//创建服务端socket，绑定到某个端口，并且开始监听客户端连接
int startup(u_short *);
//请求方法未在服务端实现，返回对应错误信息给客户端
void unimplemented(int);

/**********************************************************************/
/* A request has caused a call to accept() on the server port to
 * return.  Process the request appropriately.
 * Parameters: the socket connected to the client */
/**********************************************************************/
/* 一个连接请求导致在服务端端口上调用acceept()
 * 服务端合理的处理该请求
 * 参数：连接到服务端的客户端socket*/ 
/**********************************************************************/
void accept_request(int client)
{
 char buf[1024];
 int numchars;
 char method[255];
 char url[255];
 char path[512];
 size_t i, j;
 struct stat st;
 int cgi = 0;      /* becomes true if server decides this is a CGI
                    * program */
 char *query_string = NULL;

 //读取HTTP请求的第一行：方法 空格 URL 空格 版本 回车换行。相关信息通过空格分隔
 //HTTP协议格式，参见 http://www.xumenger.com/network-1-20161021/
 numchars = get_line(client, buf, sizeof(buf));
 i = 0; j = 0;
 //因为第一行的方法、URL、版本通过空格分隔，所以这里需要使用ISspace截取这三个信息
 while (!ISspace(buf[j]) && (i < sizeof(method) - 1))
 {
	//获取HTTP方法
  method[i] = buf[j];
  i++; j++;
 }
 method[i] = '\0';

 //strcasecmp()用忽略大小写比较字符串，如果相等，则返回0(False)，否则返回非0(True)
 if (strcasecmp(method, "GET") && strcasecmp(method, "POST"))
 {
	//如果既不是GET方法，又不是POST方法，则返回错误：服务端不支持的方法
  unimplemented(client);
  return;
 }

 //如果是POST方法
 if (strcasecmp(method, "POST") == 0)
  cgi = 1;

 i = 0;
 while (ISspace(buf[j]) && (j < sizeof(buf)))
  j++;
 while (!ISspace(buf[j]) && (i < sizeof(url) - 1) && (j < sizeof(buf)))
 {
	//获取请求URL
  url[i] = buf[j];
  i++; j++;
 }
 url[i] = '\0';

 //如果是GET方法
 if (strcasecmp(method, "GET") == 0)
 {
	//GET方法的URL典型格式：http://www.example.com/index.html?name=xumenger&age=10
  query_string = url;
  while ((*query_string != '?') && (*query_string != '\0'))
   query_string++;
  if (*query_string == '?')
  {
   cgi = 1;
   *query_string = '\0';
   query_string++;
  }
 }

 //sprintf()用于将格式化得到的字符串赋给第一个参数
 sprintf(path, "htdocs%s", url);
 if (path[strlen(path) - 1] == '/')
  strcat(path, "index.html");		//strcat(dest, src)把src所指字符串添加到dest结尾处(覆盖dest结尾处的'\0')并添加'\0'
 //
 if (stat(path, &st) == -1) {
  while ((numchars > 0) && strcmp("\n", buf))  /* read & discard headers */
   numchars = get_line(client, buf, sizeof(buf));
  not_found(client);
 }
 else
 {
  if ((st.st_mode & S_IFMT) == S_IFDIR)
   strcat(path, "/index.html");
  if ((st.st_mode & S_IXUSR) ||
      (st.st_mode & S_IXGRP) ||
      (st.st_mode & S_IXOTH)    )
   cgi = 1;
	//如果不是执行CGI，则读取文件中的信息，打包成HTTP应答消息，发送回客户端
  if (!cgi)
   serve_file(client, path);
	//否则执行CGI程序，动态生成HTTP内容返回给客户端
  else
   execute_cgi(client, path, method, query_string);
 }

 //因为HTTP底层并不是长连接的TCP，所以将请求处理完成后就要关闭TCP连接
 close(client);
}

/**********************************************************************/
/* Inform the client that a request it has made has a problem.
 * Parameters: client socket */
/**********************************************************************/
/* 通知客户端，HTTP请求导致了一个错误
 * 参数：客户端连接socket*/
/**********************************************************************/
void bad_request(int client)
{
 char buf[1024];

 //因为出现了错误，所以要在HTTP应答中给客户端返回一个错误的应答
 //这里有必要注意一下HTTP协议应答部分的报文头格式！
 //http://www.xumenger.com/network-1-20161021/
 
 //HTTP版本 应答代码 应答信息
 sprintf(buf, "HTTP/1.0 400 BAD REQUEST\r\n");
 send(client, buf, sizeof(buf), 0);
 //应答内容的类型，是html还是？
 sprintf(buf, "Content-type: text/html\r\n");
 send(client, buf, sizeof(buf), 0);
 //HTTP应答中专门有'\r\n'隔开报文头和报文主体
 sprintf(buf, "\r\n");
 send(client, buf, sizeof(buf), 0);
 //HTTP应答报文体，也就是HTML格式的字符串信息，一般是在浏览器上渲染展示
 sprintf(buf, "<P>Your browser sent a bad request, ");
 send(client, buf, sizeof(buf), 0);
 sprintf(buf, "such as a POST without a Content-Length.\r\n");
 send(client, buf, sizeof(buf), 0);
}

/**********************************************************************/
/* Put the entire contents of a file out on a socket.  This function
 * is named after the UNIX "cat" command, because it might have been
 * easier just to do something like pipe, fork, and exec("cat").
 * Parameters: the client socket descriptor
 *             FILE pointer for the file to cat */
/**********************************************************************/
/* 将文件中的数据发送给客户端
 * 参数：客户端socket
 *			 对应的文件指针*/
/**********************************************************************/
void cat(int client, FILE *resource)
{
 //定义一个内存缓冲区，大小是1024字节
 char buf[1024];

 //从resource指向的文件中读取sizeof(buf)字节数据到buf所在的内存中
 fgets(buf, sizeof(buf), resource);
 //如果没有读到文件尾就一直读
 while (!feof(resource))
 {
  //每读1024字节，就调用send方法发给客户端
	//比如可以将html文件中的数据发送给客户端
  send(client, buf, strlen(buf), 0);
  fgets(buf, sizeof(buf), resource);
 }
}

/**********************************************************************/
/* Inform the client that a CGI script could not be executed.
 * Parameter: the client socket descriptor. */
/**********************************************************************/
/* 通知客户端CGI脚本无法被执行
 * 参数：客户端socket*/
/**********************************************************************/
void cannot_execute(int client)
{
 char buf[1024];

 //通过HTTP应答返回500错误码
 sprintf(buf, "HTTP/1.0 500 Internal Server Error\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "Content-type: text/html\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "<P>Error prohibited CGI execution.\r\n");
 send(client, buf, strlen(buf), 0);
}

/**********************************************************************/
/* Print out an error message with perror() (for system errors; based
 * on value of errno, which indicates system call errors) and exit the
 * program indicating an error. */
/**********************************************************************/
/* 主要用于服务端出错时，退出程序并输出错误信息*/
/*********************************************************************/
void error_die(const char *sc)
{
 //将上一个函数发生错误的原因输出到标准设备stderr
 //参数sc所指向的字符串会先打印，后面再加上错误原因字符串
 perror(sc);
 exit(1);
}

/**********************************************************************/
/* Execute a CGI script.  Will need to set environment variables as
 * appropriate.
 * Parameters: client socket descriptor
 *             path to the CGI script */
/**********************************************************************/
/* 执行CGI脚本，需要设置环境变量
 * 参数：客户端socket、CGI脚本路径 */
/**********************************************************************/
void execute_cgi(int client, const char *path,
                 const char *method, const char *query_string)
{
 char buf[1024];
 int cgi_output[2];
 int cgi_input[2];
 pid_t pid;
 int status;
 int i;
 char c;
 int numchars = 1;
 int content_length = -1;

 buf[0] = 'A'; buf[1] = '\0';
 //如果是HTTP的GET方法
 if (strcasecmp(method, "GET") == 0)
  while ((numchars > 0) && strcmp("\n", buf))  /* read & discard headers */
	 //get_line方法，是诸行读取HTTP请求的内容
   numchars = get_line(client, buf, sizeof(buf));
 //如果是POST方法
 else    /* POST */
 {
  numchars = get_line(client, buf, sizeof(buf));
  while ((numchars > 0) && strcmp("\n", buf))
  {
   buf[15] = '\0';
   if (strcasecmp(buf, "Content-Length:") == 0)
		//atoi是把一个字符串转换成整型数的一个函数
		//对于atoi，atoi("123")->123；atoi("abc")->0
    content_length = atoi(&(buf[16]));
   numchars = get_line(client, buf, sizeof(buf));
  }
  if (content_length == -1) {
   bad_request(client);
   return;
  }
 }

 sprintf(buf, "HTTP/1.0 200 OK\r\n");
 send(client, buf, strlen(buf), 0);

 //pipe()
 if (pipe(cgi_output) < 0) {
  cannot_execute(client);
  return;
 }
 if (pipe(cgi_input) < 0) {
  cannot_execute(client);
  return;
 }

 //创建子进程
 //如果fork()返回值小于0，说明创建子进程失败
 if ( (pid = fork()) < 0 ) {
  cannot_execute(client);
  return;
 }
 //fork()分别在父子进程中返回值，在父进程中返回新建的子进程id，在子进程中返回0
 //如果返回0，则进入子进程的逻辑
 if (pid == 0)  /* child: CGI script */
 {
  char meth_env[255];
  char query_env[255];
  char length_env[255];

	//dup和dup2是两个非常有用的调用，它们的作用都是用来复制一个文件的描述符
	//它们经常用来重定向进程的stdin、stdout、stderr
	//int dup(int oldfd)
	//int dup2(int oldfd, int targetfd)	返回成功是targetfd会变成oldfd的复制品
	//	两个文件描述符都指向同一个文件，并且是函数第一个参数指向的文件
	//dup2(oldfd, 1)用新打开的文件描述符替换掉由1代表的文件描述符（1也就是stdout）
	// 然后任何写到stdout的东西，现在都将写入oldfd指向的文件中
	// 需要注意的是，dup2函数在复制oldfd之后，就立即将其关闭，但不会关掉新打开的文件描述符
  dup2(cgi_output[1], 1);		//1时stdout
  dup2(cgi_input[0], 0);		//0是stdin
  close(cgi_output[0]);
  close(cgi_input[1]);
  sprintf(meth_env, "REQUEST_METHOD=%s", method);
	//putenv可以使用程序中已定义，且值形如"name=value"的字符串变量作为函数的实参
	//此时，系统将不再为该环境变量分配内存，使用的是程序定义变量的内存，
	//而是将该字符串的变量地址保存在环境中。因此该变量应该定义为全局变量
	//防止该函数退出后导致环境变量不可用
  putenv(meth_env);
  if (strcasecmp(method, "GET") == 0) {
   sprintf(query_env, "QUERY_STRING=%s", query_string);
   putenv(query_env);
  }
  else {   /* POST */
   sprintf(length_env, "CONTENT_LENGTH=%d", content_length);
   putenv(length_env);
  }
	//int execl(const char* path, const char* arg, ...)
	//  执行参数path字符串所代表的文件路径所代表的文件路径
	//  接下来的参数代表执行该文件传递过去的argv[0]、argv[1]....
	//  最后一个参数必须用空指针NULL作为结束
  execl(path, path, NULL);
  exit(0);
 //如果fork()的返回正整数，表示进入父进程的逻辑
 } else {    /* parent */
  close(cgi_output[1]);
  close(cgi_input[0]);
  if (strcasecmp(method, "POST") == 0)
   for (i = 0; i < content_length; i++) {
    recv(client, &c, 1, 0);
    write(cgi_input[1], &c, 1);
   }
  while (read(cgi_output[0], &c, 1) > 0)
   send(client, &c, 1, 0);

  close(cgi_output[0]);
  close(cgi_input[1]);
	//waitpid()会暂停目前进程的执行，直到有信号或子进程结束
  waitpid(pid, &status, 0);
 }
}

/**********************************************************************/
/* Get a line from a socket, whether the line ends in a newline,
 * carriage return, or a CRLF combination.  Terminates the string read
 * with a null character.  If no newline indicator is found before the
 * end of the buffer, the string is terminated with a null.  If any of
 * the above three line terminators is read, the last character of the
 * string will be a linefeed and the string will be terminated with a
 * null character.
 * Parameters: the socket descriptor
 *             the buffer to save the data in
 *             the size of the buffer
 * Returns: the number of bytes stored (excluding null) */
/**********************************************************************/
/* 从socket中获取一行数据，HTTP是文本协议
 *   参考http://www.xumenger.com/network-1-20161021/中知道HTTP请求头中有方法、URL等诸多头部域
 *   且每个头部都是通过回车符、换行符分开的，所以是一行一行的，这里诸行读HTTP请求可以方便解析
 * 参数：客户端socket、每读一行将字符串存储的内存地址，读到的内容的字节数*/
/**********************************************************************/
int get_line(int sock, char *buf, int size)
{
 int i = 0;
 char c = '\0';
 int n;

 while ((i < size - 1) && (c != '\n'))
 {
	//调用recv方法，每次读一个字节，这样才可以准确的判断什么时候读到了回车符、换行符
  n = recv(sock, &c, 1, 0);
  /* DEBUG printf("%02X\n", c); */
  if (n > 0)
  {
   if (c == '\r')		//'\r'是回车符
	 {
		//读到回车符后，紧接着再读一个字节，判断是不是换行符
    n = recv(sock, &c, 1, MSG_PEEK);
    /* DEBUG printf("%02X\n", c); */
    if ((n > 0) && (c == '\n'))
     recv(sock, &c, 1, 0);
    else
     c = '\n';
   }
   buf[i] = c;
   i++;
  }
  else
   c = '\n';
 }
 buf[i] = '\0';
 
 return(i);
}

/**********************************************************************/
/* Return the informational HTTP headers about a file. */
/* Parameters: the socket to print the headers on
 *             the name of the file */
/**********************************************************************/
/* 获取HTML文件对应的HTTP头信息
 * 参数：客户端socket、文件名*/
/**********************************************************************/
void headers(int client, const char *filename)
{
 char buf[1024];
 (void)filename;  /* could use filename to determine file type */

 strcpy(buf, "HTTP/1.0 200 OK\r\n");
 send(client, buf, strlen(buf), 0);
 strcpy(buf, SERVER_STRING);
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "Content-Type: text/html\r\n");
 send(client, buf, strlen(buf), 0);
 strcpy(buf, "\r\n");
 send(client, buf, strlen(buf), 0);
}

/**********************************************************************/
/* Give a client a 404 not found status message. */
/**********************************************************************/
/* 客户端传入URL，在服务端未找到对应的资源，返回404错误给客户端*/
/**********************************************************************/
void not_found(int client)
{
 char buf[1024];

 sprintf(buf, "HTTP/1.0 404 NOT FOUND\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, SERVER_STRING);
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "Content-Type: text/html\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "<HTML><TITLE>Not Found</TITLE>\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "<BODY><P>The server could not fulfill\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "your request because the resource specified\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "is unavailable or nonexistent.\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "</BODY></HTML>\r\n");
 send(client, buf, strlen(buf), 0);
}

/**********************************************************************/
/* Send a regular file to the client.  Use headers, and report
 * errors to client if they occur.
 * Parameters: a pointer to a file structure produced from the socket
 *              file descriptor
 *             the name of the file to serve */
/**********************************************************************/
/* 发送一个合格的文件内容给客户端
 * 参数：客户端socket、文件名*/
/**********************************************************************/
void serve_file(int client, const char *filename)
{
 FILE *resource = NULL;
 int numchars = 1;
 char buf[1024];

 buf[0] = 'A'; buf[1] = '\0';
 while ((numchars > 0) && strcmp("\n", buf))  /* read & discard headers */
  numchars = get_line(client, buf, sizeof(buf));

 //根据文件名打开文件
 resource = fopen(filename, "r");
 //如果打开文件错误，返回给客户端404 not found错误
 if (resource == NULL)
  not_found(client);
 else
 {
	//否则
	//先针对该问价生成HTTP报文头，发送个客户端
  headers(client, filename);
	//将文件中的文本内容发送给客户端
  cat(client, resource);
 }
 //关闭文件
 fclose(resource);
}

/**********************************************************************/
/* This function starts the process of listening for web connections
 * on a specified port.  If the port is 0, then dynamically allocate a
 * port and modify the original port variable to reflect the actual
 * port.
 * Parameters: pointer to variable containing the port to connect on
 * Returns: the socket */
/**********************************************************************/
/* 该方法创建一个socket，绑定到某个端口，专门监听该端口
 * 如果传入的端口是0，则动态分配一个端口
 * 参数：指向端口的指针
 * 返回值：创建的服务端socket*/
/**********************************************************************/
int startup(u_short *port)
{
 int httpd = 0;
 struct sockaddr_in name;

 //创建一个服务端socket
 httpd = socket(PF_INET, SOCK_STREAM, 0);
 if (httpd == -1)
  error_die("socket");
 memset(&name, 0, sizeof(name));
 name.sin_family = AF_INET;
 name.sin_port = htons(*port);
 name.sin_addr.s_addr = htonl(INADDR_ANY);
 //将服务端socket绑定到端口port上
 //如果上面name.sin_port = htons(*port)给的端口是0，就是告诉系统随机分配一个端口
 if (bind(httpd, (struct sockaddr *)&name, sizeof(name)) < 0)
  error_die("bind");

 //如果是传入的端口为0，则随机分配一个端口
 if (*port == 0)  /* if dynamically allocating a port */
 {
  int namelen = sizeof(name);
	//getsockname()用于获取一个套接字的名字
	//它用于一个已绑定或已连接的套接字，本地地址的返回
	//本调用特别适用于如下情况：未调用bind就调用connect，这时唯有getsockname调用可以获知系统内定的本地地址
  if (getsockname(httpd, (struct sockaddr *)&name, &namelen) == -1)
   error_die("getsockname");
  *port = ntohs(name.sin_port);
 }
 //监听socket
 if (listen(httpd, 5) < 0)
  error_die("listen");
 return(httpd);
}

/**********************************************************************/
/* Inform the client that the requested web method has not been
 * implemented.
 * Parameter: the client socket */
/**********************************************************************/
/* HTTP请求的方法，在服务端未对应实现，这里返回501错误信息
 * 参数：客户端socket*/
/**********************************************************************/
void unimplemented(int client)
{
 char buf[1024];

 sprintf(buf, "HTTP/1.0 501 Method Not Implemented\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, SERVER_STRING);
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "Content-Type: text/html\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "<HTML><HEAD><TITLE>Method Not Implemented\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "</TITLE></HEAD>\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "<BODY><P>HTTP request method not supported.\r\n");
 send(client, buf, strlen(buf), 0);
 sprintf(buf, "</BODY></HTML>\r\n");
 send(client, buf, strlen(buf), 0);
}

/**********************************************************************/

//程序入口
//按照下面的注解，可以看到主程序的逻辑很简单
int main(void)
{
 int server_sock = -1;
 u_short port = 0;
 int client_sock = -1;
 struct sockaddr_in client_name;
 int client_name_len = sizeof(client_name);
 pthread_t newthread;

 //服务器端，创建socket，绑定到port端口
 server_sock = startup(&port);
 printf("httpd running on port %d\n", port);

 //循环等待客户端连接、请求
 while (1)
 {
	//接受一个客户端连接请求
  client_sock = accept(server_sock,
                       (struct sockaddr *)&client_name,
                       &client_name_len);
  if (client_sock == -1)
   error_die("accept");

 /* accept_request(client_sock); */
 //针对每个客户端连接专门创建一个线程，执行accept_request方法来处理该连接相关的请求逻辑
 if (pthread_create(&newthread , NULL, accept_request, client_sock) != 0)
   perror("pthread_create");
 }

 //关闭服务端socket
 close(server_sock);

 return(0);
}
