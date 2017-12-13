import unittest,time

import os

from dquant.constants import Constants
from dquant.markets._okex_future_rest import OkexFutureRest

'''
OKEX市场request同步接口模式下的单元测试
'''
class OKEXFutureRestTestSuite(unittest.TestCase):
    """for config test case"""

    @classmethod
    def setUpClass(cls):
        os.environ[Constants.DQUANT_ENV] = "dev"

    # 测试get_ticker
    # 获取ticker（买一卖一报价）
    @unittest.skip("skipping test_get_ticker")
    def test_get_ticker(self):
        ticker = OkexFutureRest("eth_usd_this_week").get_ticker()
        print(ticker)
        return ticker

    # get_depth是获取什么信息？
    @unittest.skip("skipping test_get_depth")
    def test_get_depth(self):
        while True:
            ticker = OkexFutureRest("btc_usd_this_week").get_depth()
            print(ticker['asks'])
            time.sleep(0.5)

    @unittest.skip("skipping long")
    def test_long(self):
        ok = OkexFutureRest("eth_usd_this_week")
        ticker = ok.get_ticker()
        price = ticker['bid']['price']
        res = OkexFutureRest("eth_usd_this_week").long(amount=1, price=price)
        print(res)

    @unittest.skip("skipping short")
    def test_short(self):
        ok = OkexFutureRest("eth_usd_this_week")
        ticker = ok.get_ticker()
        price = ticker['ask']['price']
        res = OkexFutureRest("eth_usd_this_week").short(amount=1, price=price)
        print(res)

    @unittest.skip("skipping")
    def test_close_long(self):
        ok = OkexFutureRest("eth_usd_this_week")
        res = OkexFutureRest("eth_usd_this_week").close_long(amount=1)
        print(res)

    @unittest.skip("skipping")
    def test_close_short(self):
        ok = OkexFutureRest("eth_usd_this_week")
        res = OkexFutureRest("eth_usd_this_week").close_short(amount=1)
        print(res)

    # 取一个尽可能不能在市场上成交的价格
    # 然后进行报单，调用short方法进行报单
    # 因为其基本不能成交，所以就挂单在交易所
    # 然后调用delete_order方法来测试撤单！
    # @unittest.skip("skipping")
    def test_delete_order(self):
        ok = OkexFutureRest("eth_usd_this_week")
        # 尽可能不成交的价格
        p = ok.get_depth()['asks'][-1]['price']
        res = ok.short(amount=1, price=p, lever_rate=15)
        print(res)
        id = res["order_id"]
        res = ok.delete_order(order_id=id)
        print(res)

if __name__ == '__main__':
    unittest.main()
