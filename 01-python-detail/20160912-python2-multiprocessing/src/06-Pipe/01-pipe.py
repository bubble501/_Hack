# -*- coding: utf-8 -*-

#Pipe方法返回(conn1, conn2)代表一个管道的两端

#Pipe方法有duplex参数
# 如果duplex为True（默认值），则这个管道是全双工的，也就是说conn1和conn2均可收发
# 如果duplex位False，则conn1只负责接受消息，conn2只负责发送消息

#一般建议两个进程间使用管道通信时创建两个管道
# 管道1：进程1写，进程2读
# 管道2：进程1读，进程2写

#send和recv方法分别是发送和接收消息的方法
# 例如在全双工模式下，可以调用conn1.send发送消息，conn1.recv接收消息
# 如果没有消息可接收，recv方法会一直阻塞。如果管道已经被关闭，那么recv方法会抛出EOFError

#其实我们的进程化框架中主子进程间使用共享内存、事件对象实现的通信，就类似于这个管道
#进程化框架的主子进程交互的逻辑是这样的
# 主进程和子进程分别有读线程用于读取对应的共享内存中的数据
# 当共享内存中没有数据时，对应的线程就卡在wait事件对象处
# 主进程往共享内存1中写数据，然后给子进程的事件对象置set，对应子进程的读线程收到消息，就继续执行获取共享内存1中的数据
# 对应子进程给主进程发数据也一样，子进程往共享内存2中写数据，然后给主进程的事件对象置set，对应主进程的读线程收到消息，就读取共享内存2中的数据

import multiprocessing
import time

def proc1(pipe):
    while True:
        for i in xrange(10000):
            print "send: %s" % (i)
            pipe.send(i)
            time.sleep(1)

def proc2(pipe):
    while True:
        print "proc 2 recv: ", pipe.recv()
        time.sleep(1)

def proc3(pipe):
    while True:
        print "PROC 3 RECV: ", pipe.recv()
        time.sleep(1)

if __name__ == '__main__':
    pipe = multiprocessing.Pipe()
    p1 = multiprocessing.Process(target=proc1, args=(pipe[0],))
    p2 = multiprocessing.Process(target=proc2, args=(pipe[1],))
#    p3 = multiprocessing.Process(target=proc3, args=(pipe[1],))

    p1.start()
    p2.start()
#    p3.start()

    p1.join()
    p2.join()
#    p3.join()
