import asyncio, collections
import json
import logging
import hashlib
import threading, queue
from asyncio import AbstractEventLoop

import datetime
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
        self.apikey = cfg.get_config(Constants.OKEX_APIKEY)
        self.apisec = cfg.get_config(Constants.OKEX_APISEC)
        self.base_url = Constants.OKEX_FUTURE_WS_BASE
        self.contract_type = contract_type
        self.symbol = symbol
        self.q = asyncio.Queue()
        self.websocket = None
        self.depth = None
        self.trade = None
        self.hist_lenth = 10
        self.order_type = {1: 'long', 2: 'short', 3: 'close_long', 4: 'close_short'}
        self.hist={'delete': collections.OrderedDict()}
        for _type_num, _type in self.order_type.items():
            self.hist[_type] = collections.OrderedDict()
        # self.lock = threading.Lock()
        self.update_flags = {"depth": False}
        self.loop = loop # type: AbstractEventLoop
        self.static_methods_register()

    # 订阅频道
    async def sub_channel(self):
        # X值为：btc, ltc
        # sub depth
        channel = "ok_sub_future_{}_depth_{}_usd".format(self.market_currency, self.contract_type)
        # channel = "ok_sub_future_btc_depth_this_week_usd"
        message = [str({'event': 'addChannel', 'channel': channel}), self.build_message(channel=Constants.OKEX_FUTURE_LOGIN, event='login')]
        for m in message:
            await self.websocket.send(m)

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
        self.methods[Constants.OKEX_FUTURE_LOGIN] = self.login
        # 接收订单更新信息
        self.methods[Constants.OKEX_FUTURE_SUB_TRADES] = self.sub_trades


    def register_callbacks(self):
        self.methods["ok_sub_future_btc_depth_this_week_usd"] = self.update_depth
        self.methods[Constants.OKEX_FUTURE_LOGIN] = self.login
        self.methods[Constants.OKEX_FUTURE_TRADE_WS] = self.update_trade
        self.methods[Constants.OKEX_FUTURE_DELETE_ORDER_WS] = self.update_trade
        self.methods[Constants.OKEX_FUTURE_GET_ORDER_WS] = self.update_get_order
        self.methods[Constants.OKEX_FUTURE_USERINFO_WS] = self.update_get_account


    def remove_callbacks(self):
        del self.methods["ok_sub_future_btc_depth_this_week_usd"]
        del self.methods[Constants.OKEX_FUTURE_LOGIN]
        del self.methods[Constants.OKEX_FUTURE_TRADE_WS]
        del self.methods[Constants.OKEX_FUTURE_DELETE_ORDER_WS]
        del self.methods[Constants.OKEX_FUTURE_GET_ORDER_WS]
        del self.methods[Constants.OKEX_FUTURE_USERINFO_WS]


    def get_depth(self):
        self.update({"depth": False})
        return self.depth

    async def sub_trades(self, sub_trades):
        '''
        :param sub_trades:
        :return: {15411968317: {'lever_rate': 10.0, 'amount': 1.0, 'orderid': 15411968317, 'contract_id': 20171215013, 'fee': 0.0, 'contract_name': 'BTC1215', 'unit_amount': 100.0, 'price_avg': 0.0, 'type': 1, 'deal_amount': 0.0, 'contract_type': 'this_week', 'user_id': 6240992, 'system_type': 0, 'price': 16500.0, 'create_date_str': '2017-12-11 17:02:18', 'create_date': 1512982938351, 'status': 0}, 15411968630: {'lever_rate': 10.0, 'amount': 1.0, 'orderid': 15411968630, 'contract_id': 20171215013, 'fee': 0.0, 'contract_name': 'BTC1215', 'unit_amount': 100.0, 'price_avg': 0.0, 'type': 1, 'deal_amount': 0.0, 'contract_type': 'this_week', 'user_id': 6240992, 'system_type': 0, 'price': 16500.0, 'create_date_str': '2017-12-11 17:02:18', 'create_date': 1512982938625, 'status': 0}}
        '''
        order_id = sub_trades[0]["data"]['orderid']
        status = sub_trades[0]["data"]['status']
        # 成交或者撤单成功
        if status == 2 or status == -1 :
            try:
                if status == -1:
                    side = 'delete'
                else:
                    side = self.order_type[sub_trades[0]["data"]['type']]
                self.hist[side][order_id] = sub_trades[0]["data"]
                if len(self.hist[side]) > self.hist_lenth:
                    self.hist[side].popitem(last=False)
            except Exception as ex:
                logging.error('sub_trades', ex)

    async def login(self, data):
        if data[0]['data']['result'] is True:
            self.update_flags['login'] = True


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
        if trade_data[0]["data"]['result'] is True:
            self.trade = trade_data[0]["data"]
        else:
            if trade_data[0]["data"]['error_code'] != 20015:    #   订单已成交
                logging.error('Order err %s' %(trade_data[0]["data"]['error_code']))
        self.update_flags['trade'] = True

    async def update_get_order(self, order_data):
        '''
        :param order_data: [{'data': {'result': True, 'orders': [{'symbol': 'eth_usd', 'lever_rate': 10.0, 'amount': 1.0, 'fee': 0.0, 'contract_name': 'ETH1201', 'unit_amount': 10.0, 'type': 2, 'price_avg': 0.0, 'deal_amount': 0.0, 'price': 450.0, 'create_date': 1512029722000, 'order_id': 14495541683, 'status': -1}]}, 'channel': 'ok_futureusd_orderinfo'}]
        :return: {'result': True, 'orders': [{'symbol': 'eth_usd', 'lever_rate': 10.0, 'amount': 1.0, 'fee': 0.0, 'contract_name': 'ETH1201', 'unit_amount': 10.0, 'type': 2, 'price_avg': 0.0, 'deal_amount': 0.0, 'price': 450.0, 'create_date': 1512029722000, 'order_id': 14495541683, 'status': -1}]}
        '''
        self.order = order_data[0]["data"]
        self.update_flags['get_order'] = True

    async def update_get_account(self, account_data):
        '''
        :param account_data: [{'data': {'result': True, 'info': {'btc': {'balance': 0.0, 'rights': 0.0, 'contracts': []}, 'etc': {'balance': 0.0, 'rights': 0.0, 'contracts': []}, 'bch': {'balance': 0.0, 'rights': 0.0, 'contracts': []}, 'eth': {'balance': 0.10016739, 'rights': 0.10016739, 'contracts': [{'contract_type': 'this_week', 'freeze': 0.0, 'balance': 5.058e-05, 'contract_id': 20171208260, 'available': 0.10016739, 'profit': -5.058e-05, 'bond': 0.0, 'unprofit': 0.0}]}, 'ltc': {'balance': 0.0, 'rights': 0.0, 'contracts': []}}}, 'channel': 'ok_futureusd_userinfo'}]
        :return:
        '''
        if account_data[0]["data"]['result'] is True:
            self.account = account_data[0]["data"]['info']
        self.update_flags['get_account'] = True

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

    def long(self, amount, price='', lever_rate='10'):
        message = self.build_message(price=price, amount=amount, type='1', lever_rate=lever_rate, channel=Constants.OKEX_FUTURE_TRADE_WS)
        # asyncio.Queue is not thread safe
        self.loop.call_soon_threadsafe(self.q.put_nowait, message)
        self.update({'trade': False})
        return self.trade

    def short(self, amount, price='', lever_rate='10'):
        message = self.build_message(price=price, amount=amount, type='2', lever_rate=lever_rate, channel=Constants.OKEX_FUTURE_TRADE_WS)
        self.loop.call_soon_threadsafe(self.q.put_nowait, message)
        self.update({'trade': False})
        return self.trade

    def close_long(self, amount, price=''):
        message = self.build_message(price=price, amount=amount, type='3', channel=Constants.OKEX_FUTURE_TRADE_WS)
        self.loop.call_soon_threadsafe(self.q.put_nowait, message)
        self.update({'trade': False})
        return self.trade

    def close_short(self, amount, price=''):
        message = self.build_message(price=price, amount=amount, type='4', channel=Constants.OKEX_FUTURE_TRADE_WS)
        self.loop.call_soon_threadsafe(self.q.put_nowait, message)
        self.update({'trade': False})
        return self.trade

    def delete_order(self, order_id, tillOK=True):
        '''
        :param order_id:
        :return: {'result': True, 'order_id': '14435081666'}
        status(int): 订单状态(0等待成交 1部分成交 2全部成交 -1撤单 4撤单处理中)
        '''
        while True:
            try:
                message = self.build_message(order_id=order_id, channel=Constants.OKEX_FUTURE_DELETE_ORDER_WS)
                self.loop.call_soon_threadsafe(self.q.put_nowait, message)
                self.update({'trade': False})
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

    def get_order(self, order_id=None, tillOK=True):
        '''
        :param order_id:
        :param tillOK:
        :return: {'symbol': 'eth_usd', 'lever_rate': 10.0, 'amount': 1.0, 'fee': 0.0, 'contract_name': 'ETH1201', 'unit_amount': 10.0, 'type': 2, 'price_avg': 0.0, 'deal_amount': 0.0, 'price': 450.0, 'create_date': 1512029722000, 'order_id': 14495541683, 'status': -1}
        :return:{15411968317: {'lever_rate': 10.0, 'amount': 1.0, 'orderid': 15411968317, 'contract_id': 20171215013, 'fee': 0.0, 'contract_name': 'BTC1215', 'unit_amount': 100.0, 'price_avg': 0.0, 'type': 1, 'deal_amount': 0.0, 'contract_type': 'this_week', 'user_id': 6240992, 'system_type': 0, 'price': 16500.0, 'create_date_str': '2017-12-11 17:02:18', 'create_date': 1512982938351, 'status': 0}, 15411968630: {'lever_rate': 10.0, 'amount': 1.0, 'orderid': 15411968630, 'contract_id': 20171215013, 'fee': 0.0, 'contract_name': 'BTC1215', 'unit_amount': 100.0, 'price_avg': 0.0, 'type': 1, 'deal_amount': 0.0, 'contract_type': 'this_week', 'user_id': 6240992, 'system_type': 0, 'price': 16500.0, 'create_date_str': '2017-12-11 17:02:18', 'create_date': 1512982938625, 'status': 0}}
        '''
        if order_id:
            while True:
                try:
                    # message = self.build_message(channel=Constants.OKEX_FUTURE_GET_HIST_WS)
                    message = self.build_message(order_id=order_id, channel=Constants.OKEX_FUTURE_GET_ORDER_WS)
                    self.loop.call_soon_threadsafe(self.q.put_nowait, message)
                    self.update({'get_order': False})
                    order = self.order
                    if 'result' in order:
                        if order['result'] is True:
                            return order['orders'][0] if order['orders'] else None
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
        else:
            message = self.build_message(order_id='-1', status='1', current_page='1', page_length='50', channel=Constants.OKEX_FUTURE_GET_ORDER_WS)
            self.loop.call_soon_threadsafe(self.q.put_nowait, message)
            self.update({'get_order': False})
            active_order = {}
            orders = self.order
            for order in orders["orders"]:
                order_id = order['order_id']
                active_order[order_id] = order
            return active_order

    def get_account(self, coin=[]):
        message = self.build_message(channel=Constants.OKEX_FUTURE_USERINFO_WS)
        self.loop.call_soon_threadsafe(self.q.put_nowait, message)
        self.update({'get_account': False})
        try:
            if coin:
                ret = {}
                for c in coin:
                    if c.lower() in self.account:
                        ret[c.lower()] = self.account[c.lower()]
                    else:
                        ret[c.lower()] = {'balance': 0.0, 'rights': 0.0, 'contracts': []}
                return ret
            else:
                return self.account
        except Exception as ex:
            logging.exception("message")

    async def send(self):
        # 等待登录
        await asyncio.sleep(1)
        while True:
            message = await self.q.get()
            await self.websocket.send(message)

    def getHist(self):
        return self.hist

    def build_message(self, channel, event='addChannel', **kwargs):
        '''
        :param channel: subscribe channel
        :param event: default 'addChannel'
        :param kwargs: parameters
        :return:
        '''
        params = {}

        # 删除订单/获取订单
        if channel is Constants.OKEX_FUTURE_DELETE_ORDER_WS or channel is Constants.OKEX_FUTURE_GET_ORDER_WS:
            order_id = kwargs.get('order_id')
            status = kwargs.get('status', None)
            params = {'api_key': self.apikey, 'symbol': self.symbol, 'contract_type': self.contract_type, 'order_id': order_id}
            if status:
                params['status'] = status
                params['current_page'] = kwargs.get('current_page', None)
                params['page_length'] = kwargs.get('page_length', None)

        # 下单
        elif channel is Constants.OKEX_FUTURE_TRADE_WS:
            params = {'api_key': self.apikey, 'symbol': self.symbol, 'contract_type': self.contract_type,
                      'amount': str(kwargs.get('amount')), 'type': str(kwargs.get('type')), 'match_price': "1",
                      'lever_rate': str(kwargs.get('lever_rate', 10))}
            price = kwargs.get('price', None)
            if price:
                params['match_price'] = "0"
                params['price'] = str(price)

        # 默认参数
        else:
            params = {'api_key': self.apikey}

        params['sign'] = self.buildMySign(params, self.apisec)
        message = str({'event': event, 'channel': channel, 'parameters': params})
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
                # print(data)
                # get channel and dispatch
                channel = data[0].get("channel", None)
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
        tasks = [self.ws_handler(), self.send()]
        self.loop.run_until_complete(asyncio.wait(tasks))
        self.loop.close()
