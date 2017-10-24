# -*- coding: utf-8 -*-

#使用daemon属性
#这时候只会有主进程输出`end!!`
#因为daemon属性位True，表示主进程结束，子进程立马结束，尽管子进程可能还没有执行完成

import multiprocessing
import time

def worker(interval):
    print("work start: {0}".format(time.ctime()))
    time.sleep(interval)
    print("work end: {0}".format(time.ctime()))

if __name__ == '__main__':
    p = multiprocessing.Process(target=worker, args=(3,))
    p.daemon = True
    p.start()
    print "end!!"
