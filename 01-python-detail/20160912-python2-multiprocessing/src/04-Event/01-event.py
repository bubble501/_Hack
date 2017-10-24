# -*- coding: utf-8 -*-

#Event用来实现进程间同步通信

import multiprocessing
import time

def wait_for_event(e):
    print("wait for event: starting")
    #进程在此停止执行，等待有别的地方调用e.set()才继续执行
    e.wait()
    print("wait for event: e.is_set()->" + str(e.is_set()))

def wait_for_event_timeout(e, t):
    print("wait for event timeout: starting")
    #参数t是超时时间
    #进程在此停止执行，只有等到别的地方调用e.set()
    #或者等待超过t秒后，才继续执行
    #实际中需要判断到底是哪种情况后进程再继续执行的，可能会有两个不同的代码分支
    e.wait(t)
    print("wait for event timeout: e.is_set()->" + str(e.is_set()))

if __name__ == '__main__':
    e = multiprocessing.Event()
    w1 = multiprocessing.Process(name="block", 
                                target=wait_for_event, 
                                args=(e,))
    w2 = multiprocessing.Process(name="non_block", 
                                target=wait_for_event_timeout,
                                args=(e, 2))

    w1.start()
    w2.start()

    time.sleep(3)

    e.set()
    print("main: event is set")
