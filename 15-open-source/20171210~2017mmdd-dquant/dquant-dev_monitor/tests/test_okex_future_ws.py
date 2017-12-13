import asyncio
import os, time
import unittest

'''
Python支持一种更为优雅的多进程并发方式concurrent.futures.ProcessPoolExecutor
def read(q):
    print('Get %s from queue.' % q)
    time.sleep(random.random())

def main():
    futures  = set()
    with concurrent.futures.ProcessPoolExecutor() as executor:
        for q in (chr(ord('A') + i) for i in range(26)):
            future = executor.submit(read, q)
            futures.add(future)
    try:
        for future in concurrent.futures.as_completed(futures):
            err = future.exception()
            if err is not None:
                raise err
    except KeyboardInterrupt:
        print('stoped by hand')

if __name__ == '__main__':
    main()

这里我们采用concurrent.futures.ProcessPoolExecutor对象
可以把它想象成一个进程池，子进程往里“填”
我们通过submit方法实例一个Future对象
然后把这里Future对象都填到池子futures中
这里futures是一个set对象
只要进程池里有future，就会开始执行任务
这里的read函数更为简单，只是打印字符串并休眠一会

```
try:
    for future in concurrent.futures.as_completed(futures):
```
这是等待所有子进程都执行完毕
子进程执行过程中可能抛出异常，err = future.exception()可以收集这些异常，便于后期处理

可以看出用Future对象处理多进程并发更为简洁
无论是target函数的编写、子进程的启动等
future对象还可以向使用者汇报其状态，也可以汇报执行结果或执行时的异常
'''
from concurrent.futures import ProcessPoolExecutor

import websockets
from websockets import connect

from dquant.constants import Constants
from dquant.markets._okex_future_ws import OkexFutureWs


'''
向"wss://real.okex.com:10440/websocket/okexapi"这个URL发起websocket连接
然后发送消息，send("{'event':'addChannel','channel':'ok_sub_future_btc_depth_this_week_usd'}")
...
'''
async def rawtest():
    async with websockets.connect("wss://real.okex.com:10440/websocket/okexapi") as websocket:
        await websocket.send("{'event':'addChannel','channel':'ok_sub_future_btc_depth_this_week_usd'}")
        print(await websocket.recv())
        print(await websocket.recv())
        return


async def foo():
    print("hello")


async def ping():
    import logging
    logger = logging.getLogger('websockets')
    logger.setLevel(logging.DEBUG)
    logger.addHandler(logging.StreamHandler())

    async with websockets.connect("wss://real.okex.com:10440/websocket/okexapi") as websocket:
        websocket.ping()
        websocket.close()


'''
实现websocket模式的Okex市场单元测试
'''
class OKEXFutureRestTestSuite(unittest.TestCase):
    """for config test case"""

    @classmethod
    def setUpClass(cls):
        os.environ[Constants.DQUANT_ENV] = "dev"


    def test_get_depth_background(self):
        new = asyncio.new_event_loop()
        okex = OkexFutureWs("btc_usd_this_week", new)
        okex.start()
        while True:
            x = okex.get_depth()
            print(x)

    @unittest.skip("still need work")
    def test_debug(self):
        executor = ProcessPoolExecutor(4)
        loop = asyncio.get_event_loop()
        asyncio.ensure_future(loop.run_in_executor(executor, ping()))

    @unittest.skip("skip")
    def test_speed_run_util_complete(self):
        loop = asyncio.get_event_loop()
        loop.run_until_complete(foo())

    @unittest.skip("skip")
    def test_raw_ws(self):
        import logging
        logger = logging.getLogger('websockets')
        logger.setLevel(logging.DEBUG)
        logger.addHandler(logging.StreamHandler())
        asyncio.get_event_loop().run_until_complete(
            rawtest()
        )

    # @unittest.skip("skiping get_depth")
    def test_get_depth(self):
        import logging
        logger = logging.getLogger('websockets')
        logger.setLevel(logging.DEBUG)
        logger.addHandler(logging.StreamHandler())
        ex = OkexFutureWs("btc_usd_this_week")
        while True:
            result = ex.update({"depth": True})
            print(result.depth)

    @unittest.skip("skip")
    def test_ping(self):
        asyncio.get_event_loop().run_until_complete(
            ping()
        )

    @unittest.skip("skip")
    def test_long(self):
        ex = OkexFutureWs("eth_usd_this_week")
        result = ex.long(amount=1)
        print(result)

    @unittest.skip("skip")
    def test_short(self):
        ex = OkexFutureWs("eth_usd_this_week")
        result = ex.short(amount=1)
        print(result)

    @unittest.skip("skip")
    def test_close_long(self):
        ex = OkexFutureWs("eth_usd_this_week")
        result = ex.close_long(amount=1)
        print(result)

    @unittest.skip("skip")
    def test_close_short(self):
        ex = OkexFutureWs("eth_usd_this_week")
        result = ex.close_short(amount=1)
        print(result)

    @unittest.skip("skip")
    def test_get_order(self):
        ex = OkexFutureWs("eth_usd_this_week")
        result = ex.get_order(14495541683)
        print(result)

    @unittest.skip("skip")
    def test_delete_order(self):
        ex = OkexFutureWs("eth_usd_this_week")
        depth = ex.get_depth()
        print(depth)

        result = ex.short(amount=1, price=450, lever_rate=10)
        print(result)
        id = result['order_id']

        result=ex.delete_order(order_id=id)
        print(result)

if __name__ == '__main__':
    unittest.main()
