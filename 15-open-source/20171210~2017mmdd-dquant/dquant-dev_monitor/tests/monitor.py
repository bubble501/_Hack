import time, requests, datetime, threading

import logging
from pymongo import MongoClient
import os,json
import asyncio

from dquant.constants import Constants
from dquant.markets._okex_future_rest import OkexFutureRest


os.environ[Constants.DQUANT_ENV] = "dev"

class OkexWalletAndMarket(threading.Thread):
    def __init__(self, coin):
        super(OkexWalletAndMarket, self).__init__()
        self.coin = coin
        self.mkt_btc_usd_this_week = OkexFutureRest("btc_usd_this_week")
        self.mkt_btc_usd_next_week = OkexFutureRest("btc_usd_next_week")
        self.mkt_btc_usd_quarter = OkexFutureRest("btc_usd_quarter")
        self.data = {}
        self.client = MongoClient('localhost', 27017)
        self.db = self.client.account

    def thresCoin(self, list, thres=1):
        acc = 0
        for i in range(len(list)):
            acc += float(list[i]['amount'])  # amount
            if acc > thres:
                return float(list[i]['price'])  # price
        return float(list[-1]['price'])

    async def wallet(self):
        try:
            # 获取wallet信息
            balance = await self.mkt_btc_usd_this_week.get_account(coin=self.coin)
            self.data.update(balance)
            # print(self.data)
        except Exception as ex:
            logging.error(ex)

    # 获取mkt_btc_usd_this_week的depth
    async def depth1(self):
        try:
            # get_depth()是什么函数，获取什么信息？
            # await保证可以异步获取depth的数据
            depth = await self.mkt_btc_usd_this_week.get_depth()
            # bid是指卖出外汇的价格，即我要卖出外汇，标一个价格为bid
            # ask是指买入外汇的价格，即我要买入外汇，询问得价格为ask
            bid_btc_usd_this_week = self.thresCoin(depth['bids'])
            ask_btc_usd_this_week = self.thresCoin(depth['asks'])
            self.data.update({'bid_btc_usd_this_week': bid_btc_usd_this_week, 'ask_btc_usd_this_week': ask_btc_usd_this_week})
            # print(self.data)
        except Exception as ex:
            logging.error(ex)

    # 获取mkt_btc_usd_next_week的depth
    async def depth2(self):
        try:
            depth = await self.mkt_btc_usd_next_week.get_depth()
            bid_btc_usd_next_week = self.thresCoin(depth['bids'])
            ask_btc_usd_next_week = self.thresCoin(depth['asks'])
            self.data.update({'bid_btc_usd_next_week': bid_btc_usd_next_week, 'ask_btc_usd_next_week': ask_btc_usd_next_week})
            # print(self.data)
        except Exception as ex:
            logging.error(ex)

    # 获取mkt_btc_usd_quarter的depth
    async def depth3(self):
        try:
            depth = await self.mkt_btc_usd_quarter.get_depth()
            bid_btc_usd_quarter = self.thresCoin(depth['bids'])
            ask_btc_usd_quarter = self.thresCoin(depth['asks'])
            self.data.update({'bid_btc_usd_quarter': bid_btc_usd_quarter, 'ask_btc_usd_quarter': ask_btc_usd_quarter})
            # print(self.data)
        except Exception as ex:
            logging.error(ex)

    def run(self):
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        while True:
            tasks = [self.wallet(), self.depth1(), self.depth2(), self.depth3()]
            # 这种方式可以保证在一个线程中以异步IO的方式跑多个任务tasks
            loop.run_until_complete(asyncio.wait(tasks))
            self.data.update({"_id": datetime.datetime.utcnow(), 'type': 'exchange', 'timestamp': int(time.time() * 1000)})
            # print(self.data)
            self.db.okex.insert_one(self.data)
            time.sleep(2)

# class OkexWalletAndMarket(threading.Thread):
# OkexWalletAndMarket是一个线程
okex = OkexWalletAndMarket(['btc', 'usdt'])
okex.start()  # 启动线程
okex.join()   # 等待线程结束
