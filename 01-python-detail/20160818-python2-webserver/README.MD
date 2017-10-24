## 说明

参考文章[《Python实现简单的Web服务器》](https://zhuanlan.zhihu.com/p/21323273)

通过使用Python语言实现一个Web服务器，探索HTTP协议和Web服务的基本原理，同时学习Python如何实现Web服务请求、响应、错误处理以及CGI协议，最后会使用Python面向对象进行重构

本文主要的代码参考自[《500 lines or less》](https://link.zhihu.com/?target=https%3A//github.com/aosabook/500lines/blob/master/web-server)项目，作者是Mozilla的Greg Wilson。项目代码使用MIT协议，项目文档使用 [Creative Commons Legal Code](https://link.zhihu.com/?target=http%3A//creativecommons.org/licenses/by/3.0/legalcode)协议

## 涉及到的知识点

* HTTP协议基本原理
* 简单的Web服务器架构
* Python语言的网络开发
* Web服务请求、响应以及错误处理的实现
* CGI协议的Python实现
* 使用Python面向对象思想重构代码
* 建议参考[《HTTP下午茶》](https://link.zhihu.com/?target=http%3A//www.kancloud.cn/kancloud/tealeaf-http)

## 实验环境

* 搭建Python开发虚拟环境：`virtualenv venv`
* 进入虚拟环境：`source /venv/bin/activate`
* 本实验使用httpei代替浏览器发送请求并在终端打印响应请求：`sudo apt-get install httpie`
* 使用`python servers.py`，然后在本机浏览器输入`127.0.0.1:8080`可以访问
* 除了在本地访问，比如这台机器的IP是`192.168.2.110`，在我的另一台机器的浏览器中输入`192.168.2.110:8080`也是可以跨机器访问的

## Web服务器的基本概念

1. 等待某个人连接我们的服务器并向我们发送一个HTTP请求
2. 解析该请求
3. 了解该请求希望请求的内容
4. 服务器根据请求抓取需要的数据（从服务器本地文件中或程序动态生成）
5. 将数据格式转化位请求需要的格式
6. 送回HTTP响应

步骤1、2、6的操作对于所有web应用都是一样的，这部分内容Python　标准库中的BaseHTTPServer模块可以帮助我们处理。我们只需要关注步骤3~5

## 最终总结

跟着文章将整个流程走一遍，从开始的功能实现，到最终实现重构，做一个简单总结

### 面向对象

结合代码，其中有这样的逻辑，分别用下面的方式使用都没有问题

第一种情况，是在链表中存储四个类，然后轮循获取类放在类变量case中，调用case()创建对应的对象handler

```
    Cases = [case_no_file,
      case_existing_file,
      case_directory_index_file,
      case_always_fail]

    def do_GET(self):
      try:
        # 遍历所有可能的情况
        for case in self.Cases:
          handler = case()
          if handler.test(self):
```

第二中情况，在链表中存储的是四个对象，然后轮循链表获取对象放到case中，然后直接将case对象赋值给handler

```
    Cases = [case_no_file(),
      case_existing_file(),
      case_directory_index_file(),
      case_always_fail()]

    def do_GET(self):
      try:
        # 遍历所有可能的情况
        for case in self.Cases:
          handler = case
          if handler.test(self):
```

所以在Python中类本身也是一种对象，只不过比较特殊！

### cgi功能

允许在Python程序中，在调用Python命令运行其他脚本：

```
data = subprocess.check_output(["python", path])
```

获取的返回值是其他脚本print的信息，比如time.py的内容是

```
from datetime import datetime

print '''\
    <html>
    <body>
    <p>Geterated {0}</p>
    </body>
    </html>'''.format(datetime.now())
```

那么调用

```
data = subprocess.check_output(["python", './time.py'])
```

data的值就是time.py的print输出信息

### 模板引擎

本次小项目开发的过程中有这样的代码

```
    Page = '''\
        <html>
        <body>
        <p>Hello, web!</p>
        <table>
        <tr> <td>Header</td>         <td>Value</td>         </tr>
        <tr> <td>Data and time</td>  <td>{date_time}</td>   </tr>
        <tr> <td>Client host</td>    <td>{client_host}</td> </tr>
        <tr> <td>Client_port</td>    <td>{client_port}</td> </tr>
        <tr> <td>Command</td>        <td>{command}</td>     </tr>
        <tr> <td>Path</td>           <td>{path}</td>        </tr>
        </table>
        </body>
        </html>
     '''

    def create_page(self):
        values = {
             'date_time': self.date_time_string(),
             'client_host': self.client_address[0],
             'client_port': self.client_address[1],
             'command': self.command,
             'path': self.path
         }
        page = self.Page.format(**values)   #这种方式替换字符串中的{}
        return page
```

将HTML中的{}标签通过format(**values)的方式替换为具体的值，这样就可以实现让我们的HTML中灵活的嵌入可变的信息

估计模板引擎的实现的背后原理和这个一样

### 使用Pdb调试

详细参见[《使用Pdb调试Python程序》](http://www.xumenger.com/python-pdb-20160817/)

在开发的过程中不可能保证编码一次性通过，一定会遇到各种逻辑上、语法上的问题，所以通过使用Pdb去调试，能清晰的看到程序的运行逻辑，找到问题的原因

可能因为Pdb是命令行的方式，并没有普通的IDE中的断点调试功能那么明显，但其实用习惯了之后，完全就不会有这种困扰了，万事难就难在不熟悉！

### 重构

对于这个小程序，通过重构我们发现，真正实施行为(Action)的代码逻辑可以抽象出来进行封装(封装成各种条件处理类）

而RequestHandler类或是basecase类提供了供条件处理类使用的接口，它们可以看作是一系列服务(Service)

在软件设计中，我们常常会把业务代码进行分层，将行为和服务分开，降低耦合，更利于开发维护代码

通过统一接口，以及cgi程序，代码功能扩展变得更加容易，可以专心与编写功能代码，而不用去关心其他部分

case的添加虽然仍在server代码中，但我们也可以把它放在配置文件中，由server读取配置文件

### Web 服务器

目前实现的这个服务器，只是一个菜鸟程序，距离实际真正可以生产中使用还有差距。可以试着在这个小的服务器项目的基础上扩展其他更全面的功能！

不过大致的Web服务器的原理可以通过这个菜鸟程序很好的了解

还需要对于Web、HTTP的知识持续学习

[《HTTP下午茶》](https://link.zhihu.com/?target=http%3A//www.kancloud.cn/kancloud/tealeaf-http)
