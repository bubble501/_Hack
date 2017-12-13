import asyncio
import json
import logging
import hashlib
import threading, queue
from asyncio import AbstractEventLoop

import websockets

from dquant.config import cfg
from dquant.constants import Constants
from dquant.markets.market import Market
from threading import Thread


class OkexFutureWs( Market):
    def __init__(self, meta_code,loop):
        self.contract_type = None
        self.symbol = None
        base_currency, market_currency, symbol, contract_type = self.parse_meta(meta_code)
        super().__init__(base_currency, market_currency, meta_code, cfg.get_float_config(Constants.OKEX_FEE))
        self.apikey = cfg.get_config(Constants.OKEX_APIKEY)  # apikey，估计是登录交易所需要的认证凭据
        self.apisec = cfg.get_config(Constants.OKEX_APISEC)  # apisec，估计是登录交易所需要的认证凭据

        # okex市场的websocket异步接口和request同步接口的url是不同的！
        self.base_url = Constants.OKEX_FUTURE_WS_BASE        # "wss://real.okex.com:10440/websocket/okexapi"
        self.contract_type = contract_type
        self.symbol = symbol
        self.q = queue.Queue()
        self.websocket = None
        self.depth = None
        self.trade = None
        self.update_flags = {"depth": False, 'trade': False}
        self.loop = loop # type: AbstractEventLoop
        self.static_methods_register()

    # 订阅频道
    async def sub_channel(self):
        # X值为：btc, ltc
        # sub depth
        channel = "ok_sub_future_{}_depth_{}_usd".format(self.market_currency, self.contract_type)
        # channel = "ok_sub_future_btc_depth_this_week_usd"

        """
        >>> import json
        >>> message = {'event': 'addChannel', 'channel': 'ok_sub_future_btc_depth_this_week_usd'}
        >>> print json.dumps(message)
        {"event": "addChannel", "channel": "ok_sub_future_btc_depth_this_week_usd"}
        """
        message = {'event': 'addChannel',
                   'channel': channel}
        await self.websocket.send(json.dumps(message))

    # 格式化ws传回数据
    def okex_depth_format(self, res, flag):
        result_list = []
        for ticker in res[flag]: result_list.append({
                'price': float(ticker[0]),
                'amount': float(ticker[1])
            })

        if flag == "asks":  # 卖单从小到大
            result_list.sort(key=lambda x: x['price'])
        else:  # 买单从大到小
            result_list.sort(key=lambda x: x['price'], reverse=True)
        return result_list

    # 持续接收ws传回数据直到flag全部为True
    async def processing(self, flags):
        self.unset_flags(flags)
        while True:
            try:
                data = await self.websocket.recv()
                data = json.loads(data)
                # get channel and dispatch
                channel = data[0]["channel"]
                if channel in self.methods:
                    if self.methods[channel] != None:
                        await self.methods[channel](data)
                else:
                    pass
                if (self.check_flags(flags)):
                    break
            except Exception as e:
                logging.exception("message")

    def static_methods_register(self):
        self.methods["ok_sub_future_btc_depth_this_week_usd"] = self.update_depth

    def register_callbacks(self):
        self.methods["ok_sub_future_btc_depth_this_week_usd"] = self.update_depth
        self.methods[Constants.OKEX_FUTURE_TRADE_WS] = self.update_trade
        self.methods[Constants.OKEX_FUTURE_DELETE_ORDER_WS] = self.update_trade
        self.methods[Constants.OKEX_FUTURE_GET_ORDER_WS] = self.update_get_order

    def remove_callbacks(self):
        del self.methods["ok_sub_future_btc_depth_this_week_usd"]
        del self.methods[Constants.OKEX_FUTURE_TRADE_WS]
        del self.methods[Constants.OKEX_FUTURE_DELETE_ORDER_WS]
        del self.methods[Constants.OKEX_FUTURE_GET_ORDER_WS]

    def get_depth(self):
        return self.depth

    async def update_depth(self, depth_data):
        list_of_ask = self.okex_depth_format(depth_data[0]["data"], "asks")
        list_of_bid = self.okex_depth_format(depth_data[0]["data"], "bids")
        self.depth = {"asks": list_of_ask, 'bids': list_of_bid}
        self.update_flags["depth"] = True

    async def update_trade(self, trade_data):
        '''
        :param trade_data: [{'data': {'result': True, 'order_id': 14420556515}, 'channel': 'ok_futuresusd_trade'}]
        :return: {'result': True, 'order_id': 14420556515}
        '''
        self.trade = trade_data[0]["data"]
        self.update_flags['trade'] = True

    async def update_get_order(self, order_data):
        '''
        :param order_data: [{'data': {'result': True, 'orders': [{'symbol': 'eth_usd', 'lever_rate': 10.0, 'amount': 1.0, 'fee': 0.0, 'contract_name': 'ETH1201', 'unit_amount': 10.0, 'type': 2, 'price_avg': 0.0, 'deal_amount': 0.0, 'price': 450.0, 'create_date': 1512029722000, 'order_id': 14495541683, 'status': -1}]}, 'channel': 'ok_futureusd_orderinfo'}]
        :return: {'result': True, 'orders': [{'symbol': 'eth_usd', 'lever_rate': 10.0, 'amount': 1.0, 'fee': 0.0, 'contract_name': 'ETH1201', 'unit_amount': 10.0, 'type': 2, 'price_avg': 0.0, 'deal_amount': 0.0, 'price': 450.0, 'create_date': 1512029722000, 'order_id': 14495541683, 'status': -1}]}
        '''
        self.order = order_data[0]["data"]
        self.update_flags['get_order'] = True

    def update(self, flags):
        '''
        :param flags: {"depth": True, 'trade': False}
        :return:
        '''
        self.unset_flags(flags)
        self.register_callbacks()

        while True:
            asyncio.sleep(1)
            if (self.check_flags(flags)):
                break
        self.remove_callbacks()
        return self

    # 调用trade_message按照交易所接口打包请求报文
    # 将请求放入队列self.q.put(message)
    # 调用update方法
    def long(self, amount, price='', lever_rate='10'):
        message = self.trade_message(price=price, amount=amount, type='1', lever_rate=lever_rate)
        self.q.put(message)
        self.update()
        return self.trade

    def short(self, amount, price='', lever_rate='10'):
        message = self.trade_message(price=price, amount=amount, type='2', lever_rate=lever_rate)
        self.q.put(message)
        self.update()
        return self.trade

    def close_long(self, amount, price=''):
        message = self.trade_message(price=price, amount=amount, type='3')
        self.q.put(message)
        self.update()
        return self.trade

    def close_short(self, amount, price=''):
        message = self.trade_message(price=price, amount=amount, type='4')
        self.q.put(message)
        self.update()
        return self.trade

    def delete_order(self, order_id, tillOK=True):
        '''
        :param order_id:
        :return: {'result': True, 'order_id': '14435081666'}
        status(int): 订单状态(0等待成交 1部分成交 2全部成交 -1撤单 4撤单处理中)
        '''
        while True:
            try:
                message = self.trade_message(price=None, amount=None, type='delete_order', order_id=order_id)
                self.q.put(message)
                self.update()
                if tillOK:
                    order = self.get_order(order_id)
                    if 'status' in order:
                        status = order['status']
                        # 仍然在待成交状态，继续cancel order
                        if status == 0 or status == 1:
                            logging.error(order)
                            continue
                        else:
                            return self.trade
            except Exception as e:
                logging.exception("message")
                if tillOK:
                    continue
                return None

    def get_order(self, order_id, tillOK=True):
        '''
        :param order_id:
        :param tillOK:
        :return: {'symbol': 'eth_usd', 'lever_rate': 10.0, 'amount': 1.0, 'fee': 0.0, 'contract_name': 'ETH1201', 'unit_amount': 10.0, 'type': 2, 'price_avg': 0.0, 'deal_amount': 0.0, 'price': 450.0, 'create_date': 1512029722000, 'order_id': 14495541683, 'status': -1}
        '''
        while True:
            try:
                message = self.trade_message(price=None, amount=None, type='get_order', order_id=order_id)
                self.q.put(message)
                self.update()
                order = self.order
                if 'result' in order:
                    if order['result'] is True:
                        return order['orders'][0]
                if tillOK:
                    continue
                else:
                    logging.error(order)
                    break
            except Exception as e:
                logging.exception("message")
                if tillOK:
                    continue
                return None


    # get_order、close_short等都是调用trade_message按照交易所规定的接口来进行格式化
    # 然后将格式化的消息放入队列中
    # send方法则是将队列中的消息取出来，调用websocket的send方法发给交易所
    async def send(self):
        while not self.q.empty():
            message = self.q.get()
            await self.websocket.send(message)


    # 按照交易所接口格式打包消息
    def trade_message(self, price, amount, type, lever_rate='10', **args):
        self.update_flags = {"depth": False, 'get_order': False} if type == 'get_order' else {"depth": False,
                                                                                              'trade': False}

        if type in ['delete_order', 'get_order']:
            order_id = args.get('order_id')
            params = {'api_key': self.apikey, 'symbol': self.symbol, 'contract_type': self.contract_type,
                      'order_id': order_id}
            params['sign'] = self.buildMySign(params, self.apisec)
            message = str({'event': 'addChannel',
                           'channel': Constants.OKEX_FUTURE_DELETE_ORDER_WS if type == 'delete_order' else Constants.OKEX_FUTURE_GET_ORDER_WS,
                           'parameters': params})
        else:
            match_price = "0" if price else "1"
            params = {'api_key': self.apikey, 'symbol': self.symbol, 'contract_type': self.contract_type,
                      'amount': str(amount),
                      'type': type, 'match_price': match_price, 'lever_rate': str(lever_rate)}
            if price:
                params['price'] = str(price)
            sign = self.buildMySign(params, self.apisec)
            params['sign'] = sign
            message = str({'event': 'addChannel', 'channel': Constants.OKEX_FUTURE_TRADE_WS, 'parameters': params})
        return message

    def buildMySign(self, params, secretKey):
        sign = ''
        for key in sorted(params.keys()):
            sign += key + '=' + str(params[key]) + '&'
        return hashlib.md5((sign + 'secret_key=' + secretKey).encode("utf-8")).hexdigest().upper()

    def parse_meta(self, meta_code):
        meta_table = {"btc_usd_this_week": ("btc", "usd", "btc_usd", "this_week"),
                      "eth_usd_this_week": ("eth", "usd", "eth_usd", "this_week"), }
        return meta_table[meta_code]

    async def ws_init(self):
        await self.sub_channel()

    async def ws_handler(self):
        '''
        handle task in backthread
        :return:
        '''
        while True:
            try:
                data = await self.websocket.recv()
                data = json.loads(data)
                # get channel and dispatch
                channel = data[0]["channel"]
                if channel in self.methods:
                    if self.methods[channel] != None:
                        await self.methods[channel](data)
                else:
                    pass
            except Exception as e:
                logging.exception("message")

    def run(self):
        asyncio.set_event_loop(self.loop)
        self.loop.run_until_complete(self.keep_connect())
        self.loop.run_until_complete(self.ws_init())
        self.loop.run_until_complete(self.ws_handler())
