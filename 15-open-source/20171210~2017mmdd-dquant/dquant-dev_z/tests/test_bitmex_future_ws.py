import unittest,json,hmac,hashlib,time
from urllib.parse import urlparse
from websocket import create_connection

import os

from dquant.constants import Constants
from dquant.markets._bitmex_future_ws import BitmexFutureWs





class BitmexFutureTestSuite(unittest.TestCase):
    """for config test case"""

    @classmethod
    def setUpClass(cls):
        os.environ[Constants.DQUANT_ENV] = "dev"

    @unittest.skip("skipping depth")
    def test_get_depth(self):
        # import logging
        # logger = logging.getLogger('websockets')
        # logger.setLevel(logging.DEBUG)
        # logger.addHandler(logging.StreamHandler())
        ex = BitmexFutureWs("btc_usd")
        result = ex.update()
        print(result.depth)

    @unittest.skip("skipping")
    def test_long(self):
        '''
        :return: [{'orderID': '295b6c50-74bd-1444-4905-9259d9d04903', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Buy', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10564.5, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Market', 'timeInForce': 'ImmediateOrCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.466e-05, 'cumQty': 1, 'avgPx': 10564, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T07:54:28.425Z', 'timestamp': '2017-11-29T07:54:28.425Z'}, {'orderID': '4476743b-65c8-78da-0b5e-c13c7a9c66f6', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Buy', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10570, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Market', 'timeInForce': 'ImmediateOrCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.461e-05, 'cumQty': 1, 'avgPx': 10569.5, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T07:55:13.327Z', 'timestamp': '2017-11-29T07:55:13.327Z'}, {'orderID': '9d1eb67e-04b8-f26e-a8a1-bce80aa7b173', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Sell', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10571, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Market', 'timeInForce': 'ImmediateOrCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.463e-05, 'cumQty': 1, 'avgPx': 10571, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T07:55:15.742Z', 'timestamp': '2017-11-29T07:55:15.742Z'}, {'orderID': '3ca9010b-1864-474b-fb05-54d6ee9bea52', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Sell', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10566, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Market', 'timeInForce': 'ImmediateOrCancel', 'execInst': 'Close', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.463e-05, 'cumQty': 1, 'avgPx': 10566.5, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T07:57:03.609Z', 'timestamp': '2017-11-29T07:57:03.609Z'}, {'orderID': 'f7a79a27-b321-1247-12a8-b95ce17af83f', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': '', 'simpleOrderQty': None, 'orderQty': None, 'price': None, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Market', 'timeInForce': 'ImmediateOrCancel', 'execInst': 'Close', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Canceled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 0, 'cumQty': 0, 'avgPx': None, 'multiLegReportingType': 'SingleSecurity', 'text': 'Canceled: Order had execInst of Close or ReduceOnly but current position is 0\nSubmitted via API.', 'transactTime': '2017-11-29T07:59:07.711Z', 'timestamp': '2017-11-29T07:59:07.711Z'}, {'orderID': 'd4e91f38-514e-d4cb-89df-9cf95266f693', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Sell', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10771.5, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Limit', 'timeInForce': 'GoodTillCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.284e-05, 'cumQty': 1, 'avgPx': 10771, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T10:02:03.591Z', 'timestamp': '2017-11-29T10:03:01.806Z'}, {'orderID': '08601c74-334a-8d6c-503c-541761cdb00e', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Buy', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10759, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Market', 'timeInForce': 'ImmediateOrCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.284e-05, 'cumQty': 1, 'avgPx': 10758.5, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T10:39:33.377Z', 'timestamp': '2017-11-29T10:39:33.377Z'}]
        '''
        ex = BitmexFutureWs("btc_usd")
        result = ex.long(amount=1, lever=2)
        print(result)

    @unittest.skip("skipping")
    def test_short(self):
        '''
        :return: {'orderID': 'd4e91f38-514e-d4cb-89df-9cf95266f693', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Sell', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10771.5, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Limit', 'timeInForce': 'GoodTillCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'New', 'triggered': '', 'workingIndicator': True, 'ordRejReason': '', 'simpleLeavesQty': 0.0001, 'leavesQty': 1, 'simpleCumQty': 0, 'cumQty': 0, 'avgPx': None, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T10:02:03.591Z', 'timestamp': '2017-11-29T10:02:03.591Z'}
        '''
        ex = BitmexFutureWs("btc_usd")
        result = ex.short(amount=1)
        print(result)

    @unittest.skip("skipping")
    def test_close_short(self):
        ex = BitmexFutureWs("btc_usd")
        result = ex.close_short(amount=1)
        print(result)

    @unittest.skip("skipping")
    def test_close_long(self):
        ex = BitmexFutureWs("btc_usd")
        result = ex.close_long(amount=1)
        print(result)

    @unittest.skip("skipping")
    def test_get_order(self):
        '''
        :return: [{'orderID': '295b6c50-74bd-1444-4905-9259d9d04903', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Buy', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10564.5, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Market', 'timeInForce': 'ImmediateOrCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.466e-05, 'cumQty': 1, 'avgPx': 10564, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T07:54:28.425Z', 'timestamp': '2017-11-29T07:54:28.425Z'}]
        '''
        ex = BitmexFutureWs("btc_usd")
        res = ex.get_order()
        print(res)

    def test_delete_order(self):
        '''
        :return: [{'orderID': 'd4e91f38-514e-d4cb-89df-9cf95266f693', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Sell', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10771.5, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Limit', 'timeInForce': 'GoodTillCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.284e-05, 'cumQty': 1, 'avgPx': 10771, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T10:02:03.591Z', 'timestamp': '2017-11-29T10:03:01.806Z', 'error': 'Unable to cancel order due to existing state: Filled'}]
        '''
        ex = BitmexFutureWs("btc_usd")
        result = ex.update()
        # 尽量不成交
        p = result.depth['asks'][-1]['price']
        result = ex.short(amount=1, price=p)
        print(result)
        orderID = result['orderID']
        result = ex.delete_order(orderID=orderID)
        print(result)




if __name__ == '__main__':
    unittest.main()
