import asyncio
import json
import logging

import websockets
from dquant.config import cfg
from dquant.constants import Constants
from dquant.markets.market import Market
import requests, hmac, hashlib, time
from urllib.parse import urlparse, urlencode
from requests import RequestException


class BitmexFutureWs(Market):
    def __init__(self, meta_code):
        base_currency, market_currency, symbol = self.parse_meta(meta_code)
        super().__init__(base_currency, market_currency, meta_code, cfg.get_float_config(Constants.BITMEX_FEE))
        self.apikey = cfg.get_config(Constants.BITMEX_APIKEY)
        self.apisec = cfg.get_config(Constants.BITMEX_APISEC)
        self.base_url = Constants.BITMEX_FUTURE_WS_BASE
        self.symbol = symbol
        self.timeout = Constants.OK_HTTP_TIMEOUT
        self.update_flags = {"depth": False}
        self.websocket = None
        self.session = requests.session()

    async def sub_channel(self):
        # sub depth
        args = "orderBook10:{}".format(self.symbol)
        message = {'op': 'subscribe',
                   'args': args}
        await self.websocket.send(json.dumps(message))

    def bitmex_depth_format(self, res, flag):
        result_list = []
        for ticker in res[flag]:
            result_list.append({
                'price': float(ticker[0]),
                'amount': float(ticker[1])
            })
        if flag == "asks":  # 卖单从小到大
            result_list.sort(key=lambda x: x['price'])
        else:  # 买单从大到小
            result_list.sort(key=lambda x: x['price'], reverse=True)

        return result_list

    def get_depth(self):
        return self.depth

    async def processing(self, flags):
        # 将所有Flag设置为False
        self.unset_flags(flags)
        while True:
            try:
                data = await self.websocket.recv()
                data = json.loads(data)
                # get channel and dispatch
                if 'table' in data:
                    channel = data["table"]
                    if (self.methods[channel] != None):
                        await self.methods[channel](data)
                    else:
                        pass
                # 反复经过self.methods[channel]处理ws返回的数据，直到所有Flag为真（即全部更新完毕）
                if (self.check_flags(flags)):
                    break
            except Exception as e:
                logging.exception("message")

    # 注册函数，用完删除
    def register_callbacks(self):
        self.methods["orderBook10"] = self.update_depth

    # 更新depth，并且更新flag状态
    async def update_depth(self, depth_data):
        list_of_ask = self.bitmex_depth_format(depth_data["data"][0], "asks")
        list_of_bid = self.bitmex_depth_format(depth_data["data"][0], "bids")
        self.depth = {"asks": list_of_ask, 'bids': list_of_bid}
        self.update_flags["depth"] = True

    def remove_callbacks(self):
        del self.methods["orderBook10"]

    # asyncio事件循环
    def update(self):
        loop = asyncio.get_event_loop()
        loop.run_until_complete(self.keep_connect())
        self.register_callbacks()
        loop.run_until_complete(self.processing(["depth"]))
        self.remove_callbacks()
        return self


    # Websocket 不支持新增和取消委托。 此功能请使用 REST API。 在使用时保持 HTTP 连接，请求/响应的往返时间将与 Websocket 完全相同。
    def long(self, amount, price=0, lever=None):
        '''
        :param amount: amount
        :param price: 价格，默认为Market
        :param lever: 0.01-100.与交易不在同一个API，因此如果自定义杠杆，需要等待设置杠杆对应的API返回才能下单。
        {'account': 122138, 'symbol': 'XBTUSD', 'currency': 'XBt', 'underlying': 'XBT', 'quoteCurrency': 'USD', 'commission': 0.00075, 'initMarginReq': 0.5, 'maintMarginReq': 0.005, 'riskLimit': 20000000000, 'leverage': 2, 'crossMargin': False, 'deleveragePercentile': 0.8, 'rebalancedPnl': 25, 'prevRealisedPnl': -25, 'prevUnrealisedPnl': 0, 'prevClosePrice': 9929.51, 'openingTimestamp': '2017-11-29T10:00:00.000Z', 'openingQty': 0, 'openingCost': -3, 'openingComm': 28, 'openOrderBuyQty': 0, 'openOrderBuyCost': 0, 'openOrderBuyPremium': 0, 'openOrderSellQty': 0, 'openOrderSellCost': 0, 'openOrderSellPremium': 0, 'execBuyQty': 0, 'execBuyCost': 0, 'execSellQty': 1, 'execSellCost': 9284, 'execQty': -1, 'execCost': 9284, 'execComm': -2, 'currentTimestamp': '2017-11-29T10:39:32.735Z', 'currentQty': -1, 'currentCost': 9281, 'currentComm': 26, 'realisedCost': -3, 'unrealisedCost': 9284, 'grossOpenCost': 0, 'grossOpenPremium': 0, 'grossExecCost': 9284, 'isOpen': True, 'markPrice': 10816.84, 'markValue': 9245, 'riskValue': 9245, 'homeNotional': -9.245e-05, 'foreignNotional': 1, 'posState': '', 'posCost': 9284, 'posCost2': 9284, 'posCross': 39, 'posInit': 4642, 'posComm': 11, 'posLoss': 0, 'posMargin': 4692, 'posMaint': 58, 'posAllowance': 0, 'taxableMargin': 0, 'initMargin': 0, 'maintMargin': 4653, 'sessionMargin': 0, 'targetExcessMargin': 0, 'varMargin': 0, 'realisedGrossPnl': 3, 'realisedTax': 0, 'realisedPnl': -23, 'unrealisedGrossPnl': -39, 'longBankrupt': 0, 'shortBankrupt': 0, 'taxBase': 0, 'indicativeTaxRate': 0, 'indicativeTax': 0, 'unrealisedTax': 0, 'unrealisedPnl': -39, 'unrealisedPnlPcnt': -0.0042, 'unrealisedRoePcnt': -0.0084, 'simpleQty': -0.0001, 'simpleCost': -1, 'simpleValue': -1, 'simplePnl': 0, 'simplePnlPcnt': 0, 'avgCostPrice': 10771, 'avgEntryPrice': 10771, 'breakEvenPrice': 10773.5, 'marginCallPrice': 21505, 'liquidationPrice': 21505, 'bankruptPrice': 21724.5, 'timestamp': '2017-11-29T10:39:32.735Z', 'lastPrice': 10816.84, 'lastValue': 9245}
        :return:{'orderID': '08601c74-334a-8d6c-503c-541761cdb00e', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Buy', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10759, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Market', 'timeInForce': 'ImmediateOrCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.284e-05, 'cumQty': 1, 'avgPx': 10758.5, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T10:39:33.377Z', 'timestamp': '2017-11-29T10:39:33.377Z'}
        '''
        if lever:
            res_l = self.trade(price=None,amount=None, type='set_lever', lever=lever)
            print(res_l)
        res = self.trade(price=float(price), amount=abs(float(amount)), type='long')
        return res

    def short(self, amount, price=0, lever=None):
        if lever:
            res_l = self.trade(price=None,amount=None, type='set_lever', lever=lever)
            print(res_l)
        res = self.trade(price=float(price), amount=-abs(float(amount)), type='short')
        return res

    def close_long(self, price=0, amount=None):
        '''
        :param amount: 这是个无效的参数，bitmex不能选择数量
        :param price: close_long与close_short是一样的功能，如果留空则是以市价平仓当前货币所有头寸，否则为限价
        :return:
        '''
        res = self.trade(price=float(price), amount=None, type='close_long')
        return res

    def close_short(self, price=0, amount=None):
        res = self.trade(price=float(price), amount=None, type='close_short')
        return res

    def delete_order(self, orderID=None, clOrdID=None ,tillOK=True):
        '''
        :param orderID:Either an orderID or a clOrdID must be provided.
        :param clOrdID:
        :return:[{'orderID': 'd4e91f38-514e-d4cb-89df-9cf95266f693', 'clOrdID': '', 'clOrdLinkID': '', 'account': 122138, 'symbol': 'XBTUSD', 'side': 'Sell', 'simpleOrderQty': None, 'orderQty': 1, 'price': 10771.5, 'displayQty': None, 'stopPx': None, 'pegOffsetValue': None, 'pegPriceType': '', 'currency': 'USD', 'settlCurrency': 'XBt', 'ordType': 'Limit', 'timeInForce': 'GoodTillCancel', 'execInst': '', 'contingencyType': '', 'exDestination': 'XBME', 'ordStatus': 'Filled', 'triggered': '', 'workingIndicator': False, 'ordRejReason': '', 'simpleLeavesQty': 0, 'leavesQty': 0, 'simpleCumQty': 9.284e-05, 'cumQty': 1, 'avgPx': 10771, 'multiLegReportingType': 'SingleSecurity', 'text': 'Submitted via API.', 'transactTime': '2017-11-29T10:02:03.591Z', 'timestamp': '2017-11-29T10:03:01.806Z', 'error': 'Unable to cancel order due to existing state: Filled'}]
        '''
        while True:
            try:
                if orderID or clOrdID:
                    res = self.trade(price=None, amount=None, type='delete_order', orderID=orderID, clOrdID=clOrdID)
                else:
                    logging.exception('Either an orderID or a clOrdID must be provided.')
                    return None
                if tillOK:
                    order = self.get_order(orderID=orderID, clOrdID=clOrdID)
                    ordStatus = order[0] if 'ordStatus' in order[0] else ''
                    # 订单如果仍然active，则需要重新取消
                    if ordStatus in ['New', 'Partially Filled']:
                        continue
                    # 订单已经Filled或者Canceled
                return res
            except Exception as ex:
                logging.exception("message")
                if tillOK:
                    continue
                return None

    def get_order(self, orderID=None, clOrdID=None, tillOK=True):
        while True:
            try:
                res = self.trade(price=None, amount=None, type='get_order', orderID=orderID, clOrdID=clOrdID)
                if not res or 'error' in res:
                    if tillOK == True:
                        continue
                return res
            except Exception as ex:
                logging.exception("message")
                if tillOK:
                    continue
                return None

    def trade(self, price, amount, type,  **args):
        # 删除订单，method=DELETE
        if type == 'delete_order':
            url = Constants.BITMEX_FUTURE_ORDER
            orderID = args.get('orderID', None)
            clOrdID = args.get('clOrdID', None)
            data = {'orderID': orderID} if orderID else {'clOrdID': clOrdID}
            res = self.request(url, data=data, type='delete')

        # 获取订单
        elif type == 'get_order':
            url = Constants.BITMEX_FUTURE_ORDER
            orderID = args.get('orderID', None)
            clOrdID = args.get('clOrdID', None)
            filter = json.dumps({'orderID': orderID} if orderID else {'clOrdID': clOrdID})
            params = {'symbol':self.symbol, 'filter': filter}
            res = self.request(url, params=params, type='get')

        # 设置杠杆, method=POST
        elif type == 'set_lever':
            url = Constants.BITMEX_FUTURE_LEVER
            lever = args.get('lever')
            data = {'symbol': self.symbol, 'leverage': lever}
            res = self.request(url, data=data, type='post')

        #新订单， method=POST
        else:
            if type == 'long' or type == 'short':
                url = Constants.BITMEX_FUTURE_ORDER
            else:
                url = Constants.BITMEX_FUTURE_CLOSE_POSITION
            ordType = 'Limit' if price else 'Market'
            data = {'symbol': self.symbol, 'orderQty': amount, 'ordType': ordType}
            if price:
                data['price'] = price
            res = self.request(url, data=data, type='post')

        return res

    def buildMySign(self, full_url, nonce, data, method="GET"):
        data = json.dumps(data, separators=(',', ':')) if data else ''
        parsedURL = urlparse(full_url)
        path = parsedURL.path
        if parsedURL.query:
            path = path + '?' + parsedURL.query
        message = method + path + str(nonce) + data
        signature = hmac.new(bytes(self.apisec, 'utf8'), bytes(message, 'utf8'), digestmod=hashlib.sha256).hexdigest()
        return signature

    def request(self, url, type, params=None, data=None):
        nonce = int(round(time.time() * 1000))
        full_url = url + '?' + urlencode(params) if params else url
        headers = {
            'api-nonce': str(nonce),
            'api-key': self.apikey,
            'api-signature': self.buildMySign(full_url=full_url, nonce=nonce, data=data, method=type.upper())
        }
        if type == "post":
            headers['Content-Type'] = 'application/json'
            data = json.dumps(data, separators=(',', ':')) if data else ''
            res = self.session.post(url=url, data=data, timeout=self.timeout, headers=headers)
        elif type == "get":
            res = self.session.get(url=url, params=params, timeout=self.timeout, headers=headers)
        elif type == 'delete':
            headers['Content-Type'] = 'application/json'
            data = json.dumps(data, separators=(',', ':')) if data else ''
            res = self.session.delete(url=url, data=data, timeout=self.timeout, headers=headers)


        if res.status_code == 200:
            return json.loads(res.content, encoding='utf-8')
        else:
            logging.exception("request error")
            raise RequestException("status error")

    def parse_meta(self, meta_code):
        meta_table = {"btc_usd": ("btc", "usd", "XBTUSD"), }
        return meta_table[meta_code]

    def okex_depth_format(self, param, param1):
        pass
