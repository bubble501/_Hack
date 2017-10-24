## 说明

listen系统调用的函数原型是

```
int listen(int sockfd, int backlog);
```

backlog参数提示内核监听队列的最大长度。监听队列的长度如果超过backlog，服务器将不受理新的客户连接，客户端也将收到ECONNECTFUSED错误信息

## 实验内容

下面编写一个简单的服务器程序，以研究backlog参数对listen系统调用的实际影响

## 实验顺序

* 本实验对应用到了两台机器，一台是ubuntu系统，另一台是Windows 7系统，ubuntu所在的机器是连接以太网，Windows 7所在的机器是连接无线网
* 在ubuntu上执行`ifconfig`查看以太网卡IP是192.168.2.110
* 在ubuntu上先编写完成该程序的代码，是用C++而不是C实现的
* 在ubuntu上执行`g++ -o server_read server_read.cpp`编译生成server_read二进制文件
* 在ubuntu上执行`./server_read 192.168.2.110 12345 5`监听本地的192.168.2.110的12345端口，给backlog传递典型值5
* 以下是多次重复同样的操作，并且进行记录
* (1)在Windows上打开CMD窗口，输入命令`telnet 192.168.2.110 12345`来建立TCP连接，在ubuntu上执行`netstat -nt | grep 12345`显示这一刻listen监听队列的内容

```
perfect@perfect:~$ netstat -nt | grep 12345
tcp        0      0 192.168.2.110:12345     192.168.2.102:38462     ESTABLISHED
```

* (2)在Windows上再打开CMD窗口(注意不要关闭之前的)，输入命令`telnet 192.168.2.110 12345`来建立TCP连接，在ubuntu上执行`netstat -nt | grep 12345`显示这一刻listen监听队列的内容

```
perfect@perfect:~$ netstat -nt | grep 12345
tcp        0      0 192.168.2.110:12345     192.168.2.102:38465     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38462     ESTABLISHED
```

* (3)在Windows上再打开CMD窗口(注意不要关闭之前的)，输入命令`telnet 192.168.2.110 12345`来建立TCP连接，在ubuntu上执行`netstat -nt | grep 12345`显示这一刻listen监听队列的内容

```
perfect@perfect:~$ netstat -nt | grep 12345
tcp        0      0 192.168.2.110:12345     192.168.2.102:38467     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38465     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38462     ESTABLISHED
```

* (4)在Windows上再打开CMD窗口(注意不要关闭之前的)，输入命令`telnet 192.168.2.110 12345`来建立TCP连接，在ubuntu上执行`netstat -nt | grep 12345`显示这一刻listen监听队列的内容

```
perfect@perfect:~$ netstat -nt | grep 12345
tcp        0      0 192.168.2.110:12345     192.168.2.102:38468     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38467     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38465     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38462     ESTABLISHED
```

* (5)在Windows上再打开CMD窗口(注意不要关闭之前的)，输入命令`telnet 192.168.2.110 12345`来建立TCP连接，在ubuntu上执行`netstat -nt | grep 12345`显示这一刻listen监听队列的内容

```
perfect@perfect:~$ netstat -nt | grep 12345
tcp        0      0 192.168.2.110:12345     192.168.2.102:38468     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38470     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38467     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38465     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38462     ESTABLISHED
```

* (6)在Windows上再打开CMD窗口(注意不要关闭之前的)，输入命令`telnet 192.168.2.110 12345`来建立TCP连接，在ubuntu上执行`netstat -nt | grep 12345`显示这一刻listen监听队列的内容

```
tcp        0      0 192.168.2.110:12345     192.168.2.102:38471     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38468     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38470     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38467     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38465     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38462     ESTABLISHED
```

* (7)在Windows上再打开CMD窗口(注意不要关闭之前的)，输入命令`telnet 192.168.2.110 12345`来建立TCP连接，在ubuntu上执行`netstat -nt | grep 12345`显示这一刻listen监听队列的内容

```
perfect@perfect:~$ netstat -nt | grep 12345
tcp        0      0 192.168.2.110:12345     192.168.2.102:38475     SYN_RECV
tcp        0      0 192.168.2.110:12345     192.168.2.102:38471     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38468     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38470     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38467     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38465     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38462     ESTABLISHED
```

* (8)在Windows上再打开CMD窗口(注意不要关闭之前的)，输入命令`telnet 192.168.2.110 12345`来建立TCP连接，在ubuntu上执行`netstat -nt | grep 12345`显示这一刻listen监听队列的内容

```
perfect@perfect:~$ netstat -nt | grep 12345
tcp        0      0 192.168.2.110:12345     192.168.2.102:38475     SYN_RECV
tcp        0      0 192.168.2.110:12345     192.168.2.102:38471     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38468     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38470     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38467     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38477     SYN_RECV
tcp        0      0 192.168.2.110:12345     192.168.2.102:38465     ESTABLISHED
tcp        0      0 192.168.2.110:12345     192.168.2.102:38462     ESTABLISHED
```

可见，在监听队列中，处于ESTABLISHED状态的连接只有6个(backlog值加一)，其他的连接都处于SYN_RECV状态。我们可以试着改变服务器程序的第三个参数并重新运行之，能发现同样的规律

## 简单介绍一下TCP状态转移

* 服务器通过listen系统调用，进入LISTEN状态，被动等待客户端链接，因此执行的是所谓的被动打开
* 服务器一旦监听到某个连接请求（收到同步报文段），就将该连接放入内核等待队列中，并向客户端发送SYN标志的确认报文段，此时连接处于SYN\_RECV也叫SYN\_RCVD状态
* 如果服务器成功地接收到客户端发送回的确认报文段，则该连接转移到ESTABLISHED状态，否则就一直停留在SYN\_RECV状态
* ESTABLISHED状态是连接双方能够进行双向数据传输的状态
* 对于上面的例子，我们看到只有6个连接处于ESTABLISHED状态，其他连接因为backlog参数设置的内核监听队列的最大长度的限制，所以一直处于SYN\_RECV状态
