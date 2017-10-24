# -*- coding: utf-8 -*-

#使用进程池（阻塞）

import multiprocessing
import time

def func(msg):
    print "msg: ", msg
    time.sleep(3)
    print "end"

if __name__ == '__main__':
    pool = multiprocessing.Pool(processes = 3)
    for i in xrange(4):
        msg = "hello %d" % (i)
        #维持执行的进程总数位processes，当一个进程执行完毕后会添加新的进程进去
        #apply()是阻塞的，apply_async()是非阻塞的，看第一个例子理解区别
        pool.apply(func, (msg,))

    print "Mark~ Mark~ Mark~~~~~~~~~~~~~~~~~~~"
    #close()关闭pool，使其不在接受新的任务
    pool.close()
    #调用join()之前，先调用close(),否则会出错
    #执行完close()后不会有新的进程加入到pool
    #join()函数等待所有子进程结束
    pool.join()

    print "Sub-process(es) done."
