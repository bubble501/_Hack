# -*- coding: utf-8 -*-

#Semaphore用来控制对共享资源的访问数量，例如池的最大连接数

import multiprocessing
import time

def worker(s, i):
    s.acquire()
    print(multiprocessing.current_process().name + "acquire")
    time.sleep(i)
    print(multiprocessing.current_process().name + "release\n")
    s.release()

if __name__ == '__main__':
    #参数2，表示这个Semaphore同时只能被两个进程同时acquire
    #如果其他的进程想要acquire，只能等待当前acquire的进程release
    s = multiprocessing.Semaphore(2)
    for i in range(5):
        p = multiprocessing.Process(target=worker, args=(s, i*2))
        p.start()
