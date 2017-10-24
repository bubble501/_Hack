## 说明

accept系统调用从listen监听队列中接收一个连接，函数原型是

```
int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
```

addr参数用来获取被接受连接的远端socket地址，该socket地址的长度由addrlen参数指出

accept成功时返回一个新的连接socket，该socket唯一地标识了被接受的这个连接，服务器可以通过读写该socket来与被接受连接对应的客户端通信

accept失败时返回-1并设置errno

## 实验内容

现在考虑如下情况：如果监听队列中处于ESTABLISHED状态的连接对应的客户端出现网络异常（比如掉线），或者提前退出，那么服务器对这个连接执行的accept调用是否成功？

本例编写一个简单的服务器程序来进行测试

## 实验顺序

* 本实验对应用到了两台机器，一台是ubuntu系统，另一台是Windows 7系统，ubuntu所在的机器是连接以太网，Windows 7所在的机器是连接无线网
* 在ubuntu上执行`ifconfig`查看以太网卡IP是192.168.2.110
* 在ubuntu上先编写完成该程序的代码，是用C++而不是C实现的
* 在ubuntu上执行`g++ -o server_accept server_accept.cpp`编译生成server_accept二进制文件
* 在ubuntu上执行`./server_accept 192.168.2.110 54321`监听本地的192.168.2.110的54321端口
* 然后在Windows 7所在的机器打开CMD，输入`telnet 192.168.2.110 54321`
* 启动telnet程序之后，立即关闭无线网连接（建立和断开连接的过程要在服务器启动后20秒内完成）
* 结果发现Ubuntu服务器端正常输出以下内容，说明accept调用能够成功返回

```
connetced with ip: 192.168.2.102 and port: 40418
```

* 接着在Ubuntu服务器端运行命令`netstat -nt | grep 54321`查看accept返回的连接socket的状态

```
perfect@perfect:~$ netstat -nt | grep 54321
tcp        0      0 192.168.2.110:54321     192.168.2.102:40418     ESTABLISHED
```

* netstat的输出说明，accept调用对于客户端网络断开毫不知情
* 下面重新执行上述过程，不过这次不断开客户端网络链接，而是在建立连接后立即退出客户端程序，这次accept调用同样正常返回，服务器输出信息是

```
connetced with ip: 192.168.2.102 and port: 40418
```

* 再次在服务器上执行netstat命令：

```
perfect@perfect:~$ netstat -nt | grep 54321
tcp        0      0 192.168.2.110:54321     192.168.2.102:40418     CLOSE_WAIT
```

>由此可见，accept只是从监听队列中取出连接，而不论链接处于什么状态（不管是ESTABLISHED还是CLOSE_WAIT），更不关心任何网络状态的变化
