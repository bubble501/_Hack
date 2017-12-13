这里总结常用的开发工具、平台、文档、资料等信息

## 常用工具、命令

查看某个端口信息，检查其被哪个进程（Python进程）占用

* `netstat -lpnut`命令可以查看机器的端口信息，比如被哪个进程占用
* 然后`top -p pid`可以查看这个进程的信息
* `ps -aux | grep pid`可以查看到具体是哪个python文件
* `kill -s 9 pid`可以将指定的进程强杀

git、github、gitlab版本控制

* github克隆项目到本地：`git clone git@github.com:HackerLaboratory/_Laboratory.git`
* gitlab克隆项目到本地：`git clone git@gitlab.com:depthsystem/dquant.git`
* 添加文件：`git add ???`
* 递交修改记录：`git commit -m "???"`
* 同步到远处某个分支：`git push origin 分支名`
* 切换到某个分支：`git checkout 分支名`
* 新增一个分支：`git checkout -b 分支名`，新的分支从当前所在分支拷贝所有内容
* 删除一个分支：`git branch -d 分支名`

详细可以参考[《同步管理本地git仓库和github仓库上的分支》](http://www.xumenger.com/git-github-20160804/)

## 系统工具

调试、分析工具

* Windows：WinDbg、OllyDbg、AQTime、eurekalog
* Linux：GDB、Valgrind
* 网络分析：tcpdump、WireShark

分布式系统组件：

* 数据库：MySQL
* NoSQL：Redis、MongoDB、InfluxDB、Memcached
* 服务器：Nginx、Apache、Docker

## 开发、测试平台

* [云监控平台Datadog](https://www.datadoghq.com/)
	* [Datadog控制台](https://app.datadoghq.com/)
* [HTTP Request测试平台RequestBin](https://requestb.in)
* [阿里云服务平台](https://help.aliyun.com/contact/school.htm?spm=5176.product44282.201511181.2.R7m4qU)
	* [阿里云控制台](https://home.console.aliyun.com/new)
	* [阿里云SMS服务控制台](https://dysms.console.aliyun.com/dysms.htm?spm=5176.8911205.101.190.3b317f17Nwli0I#/)
	* [阿里云短信服务平台帮助](https://help.aliyun.com/product/44282.html)
* 比特币平台，比如通过平台的接口进行量化交易
	* [okex市场|提供比特币、莱特币、以太币等数字资产的现货和合约交易](https://www.okex.com)
	* [BitMEX|比特币商品交易所 (Bitcoin Mercantile Exchange): 期货,..](https://www.bitmex.com)
* 代码托管平台可以选择github、gitlab
	* 可利用github、gitlab的webhook机制实现当有新的push就自动部署的功能

## 开发技术

* 后端开发
	* 爬虫（使用request等）抓取数据
	* websocket是在Web下的异步网络IO协议
	* flask、web.py、Django等Web开发框架
* 前端开发
	* Grafana基于JS开发，是功能齐全的度量仪表盘和图形编辑器，帮助开发人员发现问题

## 技术问题整理

>这里将一些常见的技术问题进行整理，是很好的技术能力的体现

* WebSocket是异步的IO模式，如何将其同步化？
