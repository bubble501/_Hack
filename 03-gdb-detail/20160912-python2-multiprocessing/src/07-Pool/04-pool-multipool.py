# -*- coding: utf-8 -*-

#进程池中每个进程使用不同的方法｀

import multiprocessing
import os, time, random

def Lee():
    print "\nRun task Lee-%s" % (os.getpid())   #获取当前进程的ID
    start = time.time()
    time.sleep(random.random() * 10)    #random.random()随机生成0-1之间的小数
    end = time.time()
    print 'Task Lee, run %0.2f seconds.' % (end - start)

def Marlon():
    print "\nRun task Marlon-%s" % (os.getpid())   
    start = time.time()
    time.sleep(random.random() * 40)   
    end = time.time()
    print 'Task Marlon, run %0.2f seconds.' % (end - start)

def Allen():
    print "\nRun task Allen-%s" % (os.getpid())   
    start = time.time()
    time.sleep(random.random() * 30)   
    end = time.time()
    print 'Task Allen, run %0.2f seconds.' % (end - start)

def Frank():
    print "\nRun task Frank-%s" % (os.getpid())   
    start = time.time()
    time.sleep(random.random() * 20)    
    end = time.time()
    print 'Task Frank, run %0.2f seconds.' % (end - start)



if __name__ == '__main__':
    function_list = [Lee, Marlon, Allen, Frank]
    print "parent process %s" % (os.getpid())

    pool = multiprocessing.Pool(4)
    for func in function_list:
        pool.apply_async(func)

    print "Waiting for all subprocesses done...."
    pool.close()
    pool.join()
    print "All subprocesses done"
