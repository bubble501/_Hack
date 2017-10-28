>参考自[《CURL常用命令》](http://www.cnblogs.com/gbyukg/p/3326825.html)

>另外有关于curl命令更详细的文档：[https://curl.haxx.se/docs/httpscripting.html](https://curl.haxx.se/docs/httpscripting.html)

之前在阅读《Web安全测试》的时候，书中重点有讲到如何用curl这个工具进行测试，也确实发现curl这个工具在Web领域是很强大的！

## 下载单个文件

```
$ curl http://www.xumenger.com
```

默认将输出打印到标准输出（STDOUT）中

## 将下载结果指定到某个文件

* -o：将文件保存为命令行中指定的文件名的文件中
* -O：使用URL默认的文件名保存文件到本地

```
# 指定保存到mytext.html
$ curl -o mytext.html http://www.xumenger.com/index.html
# 也可以使用重定向
$ curl http://www.xumenger.com/index.html > mytext.html

# 默认保存到index.html
$ curl -O http://www.xumenger.com/index.html
```

## 同时获取多个文件

```
$ curl -O URL1 -O URL2
```

若同时从同一网站下载多个文件时，curl会尝试重用连接

## 重定向

通过-L选项进行重定向。默认情况下curl不会发送HTTP Location Headers（重定向）。当一个被请求页面移动到另一个站点时，会发送一个HTTP Location Header作为请求，然后将请求重定向到新的地址上

例如 ，访问google.com时，会自动将地址重定向到google.com.hk上

```
$ curl http://www.google.com
<HTML>
<HEAD>
    <meta http-equiv="content-type" content="text/html;charset=utf-8">
    <TITLE>302 Moved</TITLE>
</HEAD>
<BODY>
    <H1>302 Moved</H1>
    The document has moved
    <A HREF="http://www.google.com.hk/url?sa=p&amp;hl=zh-CN&amp;pref=hkredirect&amp;pval=yes&amp;q=http://www.google.com.hk/&amp;ust=1379402837567135amp;usg=AFQjCNF3o7umf3jyJpNDPuF7KTibavE4aA">here</A>.
</BODY>
</HTML>
```

上述输出说明所请求的档案被转移到http://www.google.com.hk

这是可以通过使用-L选项进行强制重定向

```
# 让curl使用地址重定向，此时会查询google.com.hk站点
$ curl -L http://www.google.com
```

## 断点续传

通过使用-C选项可对大文件使用断点续传功能，如：

```
# 当问你件在下载完成前结束该进程
$ curl -O http://www.xumenger.com/index.html
########## 20.1%

# 通过添加-C选项继续对改文件进行下载，已经下载过的部分不再重新下载
$ curl -C -O http://www.xumenger.com/index.html
########### 21.1%
```

## 对CURL进行网络限速

通过--limit-rate选项对CURL的最大网络使用进行限制

```
# 下载速度最大不会超过1000B/second
$ curl --limit-rate 1000B -O http://www.xumenger.com/index.html
```

## 下载指定时间内修改过的文件

当下载一个文件时，可对该文件的最后修改时间进行判断，如果该文件在指定日期内修改过，就进行下载，否则不下载

可以通过-z选项来实现

```
# 若index.html在2011/12/21之后有过更新才会下载
$ curl -z 21-Dec-11 http://www.xumenger.com/index.html
```

## CURL授权

在访问需要授权的页面时，可通过-u选项提供用户名和密码进行授权

```
$ curl -u username:password URL

# 通常的做法是在命令行只输入用户名，之后会提示输入密码
# 这样可以保证在查看历史记录时不会将密码泄露
$ curl -u username URL
```

## 从FTP服务器下载文件

CURL同样支持FTP下载，若在URL中指定的是某个文件路径而非具体的要下载的文件名，CURL则会列出该目录下所有的文件名而非下载该目录下的所有文件名

```
# 列出public_html下所有的文件夹和文件
$ curl -u ftpuser:ftppass -O ftp://ftp_server/public_html/

# 下载xss.php文件
$ curl -u ftpuser:ftppass -O ftp://ftp_server/public_html/xss.php
```

## 上传文件到FTP服务器

通过-T选项可将指定的本地文件上传到FTP服务器上

```
# 将myfile.txt上传到服务器
curl -u ftpuser:ftppass -T myfile.txt ftp://ftp_server.com
```

## 获取更多信息

通过使用-v和-trace获取更多的链接信息

## 通过字典查询单词

```
# 查询bash单词的含义
$ curl dict://dict.org/d:bash

# 列出所有可用词典
$ curl dict://dict.org/show:db

# 在foldoc词典中查询bash单词的含义
$ curl dict://dict.org/d:bash:foldoc
```

## 为CURL设置代理

-x选项可以为CURL添加代理功能

```
# 指定代理主机和端口
$ curl -x proxyserver.test.com:3128 http://google.com
```

## 保存与使用网站cookie信息

```
# 将网站的cookie信息保存到file文件中
$ curl -D file http://localhost/sugarcrm/index.php

# 使用上次保存的cookie信息
$ curl -b file http://localhost/sugarcrm/index.php
```

## 传递请求数据

默认curl使用GET方式请求数据，这种方式下可直接通过URL传递数据

另外可以通过--data/-d方法指定使用POST方式传递数据

```
# GET
$ curl -u username https://api.github.com/user?access_token=XXXX

# POST
$ curl -u username -d "param1=value1&param2=value2" https://api.github.com

# 也可以指定一个文件，将该文件中的内容当做数据传递给服务器端
$ curl -data @filename https://api.github.com/authorizations
```

>默认情况下，通过POST方式传递过去的数据中若有特殊字符，首先需要将特殊字符转义在传递给服务器端，如value值中包含有空格，则需要先将空格转换成%20，如

```
curl -d "value%201" http://hostname.com
```

新版本的CURL中，提供了新的选项--data-urlencode，通过该选项提供的参数自动转义特殊字符

```
curl --data-urlencode "value 1" http://hostname.com
```

除了使用GET和POST协议外，还可以通过-X选项指定其他协议，如

```
curl -I -X DELETE https://api.github.com
```

上传文件

```
curl --form "fileupload=@filename.txt" http://hostname/resource
```

>curl的很多功能不就是自己之前学习的网络爬虫的功能吗，只不过curl在用法上简单许多！
