/*
 * (C) Radim Kolar 1997-2004
 * This is free software, see GNU Public License version 2 for
 * details.
 *
 * Simple forking WWW Server benchmark:
 *
 * Usage:
 *   webbench --help
 *
 * Return codes:
 *    0 - sucess
 *    1 - benchmark failed (server is not on-line)
 *    2 - bad param
 *    3 - internal error, fork failed
 * 
 */
#include "socket.c"
#include <unistd.h>
#include <sys/param.h>
#include <rpc/types.h>
#include <getopt.h>
#include <strings.h>
#include <time.h>
#include <signal.h>

/* values */
volatile int timerexpired=0;
int speed=0;
int failed=0;
int bytes=0;
/* globals */
int http10=1; /* 0 - http/0.9, 1 - http/1.0, 2 - http/1.1 */
/* Allow: GET, HEAD, OPTIONS, TRACE */
#define METHOD_GET 0
#define METHOD_HEAD 1
#define METHOD_OPTIONS 2
#define METHOD_TRACE 3
#define PROGRAM_VERSION "1.5"
int method=METHOD_GET;
int clients=1;
int force=0;
int force_reload=0;
int proxyport=80;
char *proxyhost=NULL;
int benchtime=30;
/* internal */
int mypipe[2];
char host[MAXHOSTNAMELEN];
#define REQUEST_SIZE 2048
char request[REQUEST_SIZE];

static const struct option long_options[]=
{
 {"force", no_argument, &force, 1}, 
 {"reload", no_argument, &force_reload, 1}, 
 {"time", required_argument, NULL, 't'}, 
 {"help", no_argument, NULL, '?'}, 
 {"http09", no_argument, NULL, '9'}, 
 {"http10", no_argument, NULL, '1'}, 
 {"http11", no_argument, NULL, '2'}, 
 {"get", no_argument, &method, METHOD_GET}, 
 {"head", no_argument, &method, METHOD_HEAD}, 
 {"options", no_argument, &method, METHOD_OPTIONS}, 
 {"trace", no_argument, &method, METHOD_TRACE}, 
 {"version", no_argument, NULL, 'V'}, 
 {"proxy", required_argument, NULL, 'p'}, 
 {"clients", required_argument, NULL, 'c'}, 
 {NULL, 0, NULL, 0}
};

/* prototypes */
//每个子进程的实际发起请求函数
static void benchcore(const char* host, const int port, const char *request);
//派生子进程，父子进程管道通信最后输出计算结果
static int bench(void);
//构造HTTP请求
static void build_request(const char *url);

//信号处理函数，时钟结束时进行调用
static void alarm_handler(int signal)
{
   timerexpired=1;
}	

//输出webbench命令的使用说明
//webbench的命令格式如下：webbench -c 500 -t 30 http://127.0.0.1:59965/
static void usage(void)
{
   fprintf(stderr, 
	"webbench [option]... URL\n"
	"  -f|--force               Don't wait for reply from server.\n"               //不等待服务器数据返回
	"  -r|--reload              Send reload request - Pragma: no-cache.\n"         //
	"  -t|--time <sec>          Run benchmark for <sec> seconds. Default 30.\n"    //-t 30：表示设置运行30s
	"  -p|--proxy <server:port> Use proxy server for request.\n"                   //使用代理服务器
	"  -c|--clients <n>         Run <n> HTTP clients at once. Default one.\n"      //-c 500：表示同时启动500个客户端，表示并发数
	"  -9|--http09              Use HTTP/0.9 style requests.\n"                    //使用0.9版本的HTTP协议进行测试
	"  -1|--http10              Use HTTP/1.0 protocol.\n"                          //使用1.0版本HTTP协议进行测试
	"  -2|--http11              Use HTTP/1.1 protocol.\n"                          //使用1.1版本HTTP协议进行测试
	"  --get                    Use GET request method.\n"                         //使用HTTP的GET请求方法进行测试
	"  --head                   Use HEAD request method.\n"                        //使用HTTP的HEAD请求方法进行测试
	"  --options                Use OPTIONS request method.\n"                     //使用HTTP的OPTIONS请求方法进行测试
	"  --trace                  Use TRACE request method.\n"                       //使用HTTP的TRACE请求方法进行测试
	"  -?|-h|--help             This information.\n"                               //输出程序的使用说明
	"  -V|--version             Display program version.\n"                        //获取程序的版本信息
	);
};

//先看主函数
int main(int argc, char *argv[])
{
 int opt=0;
 int options_index=0;
 char *tmp=NULL;

 //如果在运行webbench命令时没有其他参数，则输出webbench的使用说明，并且结束进程
 if(argc==1)
 {
	  usage();
          return 2;
 } 

 //获取用户输入的webbench详细的参数信息
 //webbench的命令格式如下：webbench -c 500 -t 30 http://127.0.0.1:59965/
 //getopt_log用于命令行解析
 while((opt=getopt_long(argc, argv, "912Vfrt:p:c:?h", long_options, &options_index))!=EOF )
 {
  switch(opt)
  {
   case  0 : break;
   case 'f': force=1; break;          //如果用户输入-f
   case 'r': force_reload=1; break;   //如果用户输入-r
   case '9': http10=0; break;         //如果用户输入-9，表示使用0.9版本的HTTP协议
   case '1': http10=1; break;         //如果用户输入-1，表示使用1.0版本的HTTP协议
   case '2': http10=2; break;         //如果用户输入-2，表示使用1.1版本的HTTP协议
   case 'V': printf(PROGRAM_VERSION"\n"); exit(0);     //如果用户输入-V，则输出版本信息
   case 't': benchtime=atoi(optarg); break;	           //如果用户输入-t，表示运行测试的秒数
   case 'p':                          //使用代理服务器
	     /* proxy server parsing server:port */
	     tmp=strrchr(optarg, ':');
	     proxyhost=optarg;
	     if(tmp==NULL)
	     {
		     break;
	     }
	     if(tmp==optarg)
	     {
		     fprintf(stderr, "Error in option --proxy %s: Missing hostname.\n", optarg);
		     return 2;
	     }
	     if(tmp==optarg+strlen(optarg)-1)
	     {
		     fprintf(stderr, "Error in option --proxy %s Port number is missing.\n", optarg);
		     return 2;
	     }
	     *tmp='\0';
	     proxyport=atoi(tmp+1); break;
   case ':':
   case 'h':
   case '?': usage(); return 2; break;
   case 'c': clients = atoi(optarg); break;     //-c表示并发数，clients = atoi(optarg)获取并发数
  }
 }
 
 //webbench的命令格式如下：webbench -c 500 -t 30 http://127.0.0.1:59965/
 //最后需要指定URL
 if(optind==argc) {
                      fprintf(stderr, "webbench: Missing URL!\n");
		      usage();
		      return 2;
                    }
 //并发数默认是1
 if(clients==0) clients=1;
 //测试时间默认是60s
 if(benchtime==0) benchtime=60;
 //输出webbench的Copyright信息
 /* Copyright */
 fprintf(stderr, "Webbench - Simple Web Benchmark "PROGRAM_VERSION"\n"
	 "Copyright (c) Radim Kolar 1997-2004, GPL Open Source Software.\n"
	 );

 //根据输入构建HTTP请求，也就是组装HTTP请求报文，传入的参数是URL
 build_request(argv[optind]);
 
 //测试完成
 /* print bench info */
 printf("\nBenchmarking: ");
 //输出HTTP方法
 switch(method)
 {
	 case METHOD_GET:
	 default:
		 printf("GET");break;
	 case METHOD_OPTIONS:
		 printf("OPTIONS");break;
	 case METHOD_HEAD:
		 printf("HEAD");break;
	 case METHOD_TRACE:
		 printf("TRACE");break;
 }
 printf(" %s", argv[optind]);
 //输出HTTP版本
 switch(http10)
 {
	 case 0: printf(" (using HTTP/0.9)");break;
	 case 2: printf(" (using HTTP/1.1)");break;
 }
 printf("\n");
 //输出并发数信息
 if(clients==1) printf("1 client");
 else
   printf("%d clients", clients);
 //输出运行时间信息
 printf(", running %d sec", benchtime);
 if(force) printf(", early socket close");
 if(proxyhost!=NULL) printf(", via proxy server %s:%d", proxyhost, proxyport);
 if(force_reload) printf(", forcing reload");
 printf(".\n");

 //
 return bench();
}

//组装HTTP请求报文，入参是URL
//参考[http://www.xumenger.com/network-1-20161021/]，了解HTTP协议的报文格式
void build_request(const char *url)
{
  char tmp[10];
  int i;

  bzero(host, MAXHOSTNAMELEN);
  bzero(request, REQUEST_SIZE);

  if(force_reload && proxyhost!=NULL && http10<1) http10=1;
  if(method==METHOD_HEAD && http10<1) http10=1;
  if(method==METHOD_OPTIONS && http10<2) http10=2;
  if(method==METHOD_TRACE && http10<2) http10=2;

  //HTTP请求报文中，首先打包请求方法
  switch(method)
  {
	  default:
	  case METHOD_GET: strcpy(request, "GET");break;
	  case METHOD_HEAD: strcpy(request, "HEAD");break;
	  case METHOD_OPTIONS: strcpy(request, "OPTIONS");break;
	  case METHOD_TRACE: strcpy(request, "TRACE");break;
  }
		  
  //紧接着HTTP报文空一格
  strcat(request, " ");

  //先对请求的URL进行合法性检查，然后在把URL打包在HTTP请求报文中
  if(NULL==strstr(url, "://"))
  {
	  fprintf(stderr, "\n%s: is not a valid URL.\n", url);
	  exit(2);
  }
  if(strlen(url)>1500)
  {
         fprintf(stderr, "URL is too long.\n");
	 exit(2);
  }
  if(proxyhost==NULL)
	   if (0!=strncasecmp("http://", url, 7)) 
	   { fprintf(stderr, "\nOnly HTTP protocol is directly supported, set --proxy for others.\n");
             exit(2);
           }
  /* protocol/host delimiter */
  i=strstr(url, "://")-url+3;
  /* printf("%d\n", i); */

  if(strchr(url+i, '/')==NULL) {
                                fprintf(stderr, "\nInvalid URL syntax - hostname don't ends with '/'.\n");
                                exit(2);
                              }
  if(proxyhost==NULL)
  {
   /* get port from hostname */
   if(index(url+i, ':')!=NULL &&
      index(url+i, ':')<index(url+i, '/'))
   {
	   strncpy(host, url+i, strchr(url+i, ':')-url-i);
	   bzero(tmp, 10);
	   strncpy(tmp, index(url+i, ':')+1, strchr(url+i, '/')-index(url+i, ':')-1);
	   /* printf("tmp=%s\n", tmp); */
	   proxyport=atoi(tmp);
	   if(proxyport==0) proxyport=80;
   } else
   {
     strncpy(host, url+i, strcspn(url+i, "/"));
   }
   // printf("Host=%s\n", host);
   strcat(request+strlen(request), url+i+strcspn(url+i, "/"));
  } else
  {
   // printf("ProxyHost=%s\nProxyPort=%d\n", proxyhost, proxyport);
   strcat(request, url);
  }

  //打包完HTTP请求的URL后，需要在HTTP请求报文中添加HTTP的版本信息
  if(http10==1)
	  strcat(request, " HTTP/1.0");
  else if (http10==2)
	  strcat(request, " HTTP/1.1");
  strcat(request, "\r\n");
  if(http10>0)
	  strcat(request, "User-Agent: WebBench "PROGRAM_VERSION"\r\n");
  if(proxyhost==NULL && http10>0)
  {
	  strcat(request, "Host: ");
	  strcat(request, host);
	  strcat(request, "\r\n");
  }
  if(force_reload && proxyhost!=NULL)
  {
	  strcat(request, "Pragma: no-cache\r\n");
  }
  if(http10>1)
	  strcat(request, "Connection: close\r\n");
  /* add empty line at end */
  if(http10>0) strcat(request, "\r\n"); 
  // printf("Req=%s\n", request);
}


/* vraci system rc error kod */
//派生子进程，父子进程用管道通信
static int bench(void)
{
  int i, j, k;	
  pid_t pid=0;
  FILE *f;

  /* check avaibility of target server */
  //父进程先创建一个Socket，发起连接，检查服务端是否能正常访问
  i=Socket(proxyhost==NULL?host:proxyhost, proxyport);
  if(i<0) { 
	   fprintf(stderr, "\nConnect to server failed. Aborting benchmark.\n");
           return 1;
         }
  //测试完成后，关闭测试Socket
  close(i);

  /* create pipe */
  //int mypipe[2];  创建管道，用于父子线程之间的通信
  if(pipe(mypipe))
  {
	  perror("pipe failed.");
	  return 3;
  }

  /* not needed, since we have alarm() in childrens */
  /* wait 4 next system clock tick */
  /*
  cas=time(NULL);
  while(time(NULL)==cas)
        sched_yield();
  */

  /* fork childs */
  //根据-c参数的并发数设置，创建对应数量的子进程
  for(i=0;i<clients;i++)
  {  
     //调用fork()创建子进程，fork有两个返回，在子进程中返回0，在父进程中返回子进程的进程id
	   pid=fork();

     //下面的逻辑重点说明一下，因为fork的逻辑还是比较特殊的
     //fork如果返回大于0的值，表示继续在父进程的内存空间，则不执行下面的sleep、break，继续按照-c创建子进程
     //如果fork返回值小于0，表示继续在父进程的内存空间，但是创建子进程失败，可能是系统资源不够，那么break跳出循环不在继续创建，而是执行后续逻辑
     //fork如果返回值是0，表示在子进程的内存空间，执行sleep，然后break跳出创建进程的循环，防止子进程再创建子子进程，进入下面的子进程逻辑
     //对于进程循环调用fork需要注意以上的情况
	   if(pid <= (pid_t) 0)
	   {
		   /* child process or error*/
	           sleep(1); /* make childs faster */
		   break;
	   }
  }

  //如果fork返回值为<0，表示创建子进程失败
  if( pid< (pid_t) 0)
  {
          fprintf(stderr, "problems forking worker no. %d\n", i);
	  perror("fork failed.");
	  return 3;
  }

  //如果fork返回值为0，表示执行子进程逻辑
  if(pid== (pid_t) 0)
  {
    /* I am a child */
    if(proxyhost==NULL)
      //benchcore 是每个子进程的实际发起请求函数
      //request是之前调用build_request构造好的HTTP请求包
      benchcore(host, proxyport, request);
         else
      benchcore(proxyhost, proxyport, request);

   /* write results to pipe */
   //子进程打开管道的一端，用于往其中写测试结果
   //管道写的数据不大于 PIPE_BUF 时，系统可以保证写的原子性 
	 f=fdopen(mypipe[1], "w");
	 if(f==NULL)
	 {
		 perror("open pipe for writing failed.");
		 return 3;
	 }
	 /* fprintf(stderr, "Child - %d %d\n", speed, failed); */
   //将当前子进程运行的测试结果打包写到管道中，对应父进程会从该管道中读并统计各个子进程的测试结果
	 fprintf(f, "%d %d %d\n", speed, failed, bytes);
   //关闭管道
	 fclose(f);
	 return 0;
  }
  //如果fork返回值>0，则进入父进程的逻辑 
  else
  {
    //父进程打开管道的另一端，读取各个子进程的测试结果
	  f=fdopen(mypipe[0], "r");
	  if(f==NULL) 
	  {
		  perror("open pipe for reading failed.");
		  return 3;
	  }
	  setvbuf(f, NULL, _IONBF, 0);
	  speed=0;
          failed=0;
          bytes=0;

    //循环统计所有子进程的测试结果
	  while(1)
	  {
      //从管道中将各个子进程的speed、failed、bytes读取出来
		  pid=fscanf(f, "%d %d %d", &i, &j, &k);
		  if(pid<2)
                  {
                       fprintf(stderr, "Some of our childrens died.\n");
                       break;
                  }
		  speed+=i;
		  failed+=j;
		  bytes+=k;
		  /* fprintf(stderr, "*Knock* %d %d read=%d\n", speed, failed, pid); */
		  if(--clients==0) break;
	  }
	  fclose(f);

  //输出统计测测试结果
  printf("\nSpeed=%d pages/min, %d bytes/sec.\nRequests: %d susceed, %d failed.\n", 
		  (int)((speed+failed)/(benchtime/60.0f)), 
		  (int)(bytes/(float)benchtime), 
		  speed, 
		  failed);
  }
  return i;
}

//每个子进程的实际发起请求函数，每个子进程调用
void benchcore(const char *host, const int port, const char *req)
{
 int rlen;
 char buf[1500];
 int s, i;
 struct sigaction sa;

 /* setup alarm signal handler */
 /* 子进程安装信号
  static void alarm_handler(int signal)
  {
     timerexpired=1;
  } 
 */
 sa.sa_handler=alarm_handler;
 sa.sa_flags=0;
 //sigaction表示为SIGALRM信号注册sa
 //也就是当前进程内如果收到SIGALRM信号，则对应执行sa.sa_handler的函数
 if(sigaction(SIGALRM, &sa, NULL))
    exit(3);

 //设置闹钟函数，benchtime是用户输入的-t信息，表示测试时间
 //这个函数的意思是，在benchtime时间后，会给当前进程发送一个SIGALRM信号
 //按照上面的说，本进程内如果收到SIGALRM信号，对应会执行alarm_handler函数，alarm_handler函数的内部就是讲timerexpired设置为1
 alarm(benchtime);

 rlen=strlen(req);
 nexttry:while(1)
 {
    //如果timerexpired为1，则退出当前循环，按照上面的说明在alarm中设置了benchtime的闹钟
    //当前进程运行benchtime后，因为alarm函数的原因会收到SIGALRM消息，收到SIGALRM消息后对应调用alarm_handler将timerexpired设为1
    //所以是通过消息的方式通知进程在benchtime后结束，所以这里判断timerexpired来及时结束子进程
    //其实和Windows、Delphi的消息机制在应用层面是想通的
    if(timerexpired)
    {
       if(failed>0)
       {
          /* fprintf(stderr, "Correcting failed by signal\n"); */
          failed--;
       }
       return;
    }
    //建立客户端socket，用于连接服务端
    s=Socket(host, port);  
    //如果创建客户端socket失败，则failed加一                        
    if(s<0) { failed++;continue;} 
    //调用write方法，向服务端发起HTTP请求，并且判断发起是否成功
    if(rlen!=write(s, req, rlen)) {failed++;close(s);continue;}
    if(http10==0) 
	    if(shutdown(s, 1)) { failed++;close(s);continue;}
    //force==0表示客户端发送请求后还要等待服务端的返回信息
    if(force==0) 
    {
            /* read all available data from socket */
	    while(1)
	    {
        if(timerexpired) break; 
        //读取服务端的应答数据，并且判断服务端是否处理成功 
	      i=read(s, buf, 1500);
              /* fprintf(stderr, "%d\n", i); */
	      if(i<0) 
              { 
                 failed++;
                 close(s);
                 goto nexttry;
              }
	       else
		       if(i==0) break;
		       else
			       bytes+=i;
	    }
    }
    //关闭客户端socket，如果关闭socket异常也作为错误处理
    if(close(s)) {failed++;continue;}
    speed++;
 }
}
