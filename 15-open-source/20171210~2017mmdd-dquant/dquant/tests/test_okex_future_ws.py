import asyncio
import os, time
import unittest
from concurrent.futures import ProcessPoolExecutor

import websockets
from websockets import connect

from dquant.constants import Constants
from dquant.markets._okex_future_ws import OkexFutureWs


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


class OKEXFutureRestTestSuite(unittest.TestCase):
    """for config test case"""

    @classmethod
    def setUpClass(cls):
        os.environ[Constants.DQUANT_ENV] = "dev"


    def test_get_depth_background(self):
        new = asyncio.new_event_loop()
        okex = OkexFutureWs("btc_usd_this_week",new)
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
