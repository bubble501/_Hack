import logging
import string

import os

from dquant.constants import Constants
from dquant.markets.market import Market
from dquant.markets.market_factory import create_markets

'''
这个类用来实现一些交易参数的比较
比如比较不同市场的买入卖出价格等

bid和ask
* bid是指卖出外汇的价格，即我要卖出外汇，标一个价格为bid
* ask是指买入外汇的价格，即我要买入外汇，询问得价格为ask
'''
class Diff():
    def __init__(self):
        self.markets = {}

    '''
    计算bitmex和okex市场的价差
    '''
    def calc_diff(self):
        try:
            while True:
                for market in self.markets:
                    self.markets[market].update()

                okex_market = self.markets["okex_future_btcusd_thisweek"] # type: Market
                bitmex_market = self.markets["bitmex_future_btcusd"] # type: Market
                okex_ticker = okex_market.get_ticker()
                bitmex_ticker = bitmex_market.get_ticker()
                ask_diff = okex_ticker['ask']['price']  - bitmex_ticker['ask']['price']
                bid_diff = okex_ticker['bid']['price']  - bitmex_ticker['bid']['price']
                print(ask_diff)
                print(bid_diff)

        except Exception as e:
            logging.exception("message")


if __name__ == '__main__':
    os.environ[Constants.DQUANT_ENV] = "dev"
    diff = Diff()
    diff.markets = create_markets(["bitmex_future_btcusd","okex_future_btcusd_thisweek"])
    diff.calc_diff()


