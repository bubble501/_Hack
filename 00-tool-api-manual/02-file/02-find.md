find命令主要用于按照一定的规则搜索系统中的某个文件。以下是一些常用的命令

`find / -name test.c` 寻找/目录下名字为test.c的文件；`find / -name *.py` 寻找/目录下.py结尾的文件

`find / -size 10c` 查找/目录下大小为10字节的文件或目录；`find / -size -10k` 查找/目录下小于10k的文件或目录；`find / -size +10G` 查找/目录下大于10G的文件或目录

更多参数和功能推荐[《Linux find 用法示例》](http://www.cnblogs.com/wanqieddy/archive/2011/06/09/2076785.html)
