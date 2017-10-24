## 说明

Python中的多线程其实并不是真正的多线程，如果想要充分地使用多核CPU的资源，在Python中大部分情况需要使用多进程

Python提供了非常好用的多进程包multiprocessing，只需要定义一个函数，Python会完成其他所有事情

借助这个包，可以轻松完成从单进程到并发执行的转换

multiprocessing支持子进程、通信和共享数据、执行不同形式的同步，提供了Process、Queue、Pipe、Lock等组件

multiprocessing模块对应的源码在`/usr/lib/python2.7/multiprocessing/`
