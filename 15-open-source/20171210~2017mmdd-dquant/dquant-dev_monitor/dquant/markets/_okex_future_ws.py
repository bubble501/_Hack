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
    def __init__(self, meta_code, loop):
        self.contract_type = None
        self.symbol = None
        base_currency, market_currency, symbol, contract_type = self.parse_meta(meta_code)
        super().__init__(base_currency, market_currency, meta_code, cfg.get_float_config(Constants.OKEX_FEE))
        self.apikey = cfg.get_config(Constants.OKEX_APIKEY)
        self.apisec = cfg.get_config(Constants.OKEX_APISEC)
        self.base_url = Constants.OKEX_FUTURE_WS_BASE        # okex市场的websocket接口的URL
        self.contract_type = contract_type
        self.symbol = symbol
        '''
        这个队列是用于存储什么数据？
        可以这样理解，websocket是一个异步接口
        当想去调用long()同步开多的时候
        在long()方法中创建一个请求放到queue中
        然后OkexFutureWs市场的线程会从queue中取出请求，处理
        然后long会持续等待，直到等待其收到应答
        这样就可以达到同步的效果
        '''
        self.q = queue.Queue()  
        self.websocket = None   # 用于和okex市场进行websocket通信
        self.depth = None
        self.trade = None
        self.update_flags = {"depth": False, 'trade': False}   # 更新标志
        self.loop = loop # type: AbstractEventLoop    # EventLoop
        '''
        注册静态方法
        def static_methods_register(self):
            self.methods["ok_sub_future_btc_depth_this_week_usd"] = self.update_depth
        '''
        self.static_methods_register()

    '''
    订阅频道

    这个应该是按照okex的websocket接口要求格式化的字符串
    然后发送给okex市场发过去，告诉okex市场
    event: addChannel，表示我要添加一个订阅的频道
    channel: ok_sub_future_btc_depth_this_week_usd，表示我要订阅哪个频道
    '''
    async def sub_channel(self):
        # X值为：btc, ltc
        # sub depth
        channel = "ok_sub_future_{}_depth_{}_usd".format(self.market_currency, self.contract_type)
        # channel = "ok_sub_future_btc_depth_this_week_usd"
        message = {'event': 'addChannel',
                   'channel': channel}
        await self.websocket.send(json.dumps(message))


    '''
    格式化ws传回数据

    这个可能就是要去看okex的接口文档了，了解收到的是什么格式的数据、是什么内容，这样才方便对于代码的理解
    这是okex的接口：https://www.okex.com/intro_apiOverview.html
    '''
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

    '''
    持续接收ws传回数据直到flag全部为True
    '''
    async def processing(self, flags):
        # unset_flag(flags)将所有flag设置为Flase
        self.unset_flags(flags)
        while True:
            try:
                # 异步等待websocket server的应答数据
                data = await self.websocket.recv()
                # 收到的数据是json格式的，这里进行解析
                data = json.loads(data)

                # get channel and dispatch
                # 这里是一个事件驱动引擎，根据收到的channel回调对应的方法处理数据
                # 比如收到一个channel是价格，那么就回调价格对应的处理方法
                # 在这些回调方法里面会更新flags的值
                channel = data[0]["channel"]
                if channel in self.methods:
                    if self.methods[channel] != None:
                        await self.methods[channel](data)
                else:
                    pass

                #check_flags方法会逐个检查flag，只有全部为真时（全部更新完毕），返回True
                if (self.check_flags(flags)):
                    break
            except Exception as e:
                logging.exception("message")


    # 在初始化__init__方法中会调用这个方法来注册回调
    def static_methods_register(self):
        self.methods["ok_sub_future_btc_depth_this_week_usd"] = self.update_depth


    # 在update方法中会调用这个方法来注册回调
    def register_callbacks(self):
        self.methods["ok_sub_future_btc_depth_this_week_usd"] = self.update_depth
        self.methods[Constants.OKEX_FUTURE_TRADE_WS] = self.update_trade
        self.methods[Constants.OKEX_FUTURE_DELETE_ORDER_WS] = self.update_trade
        self.methods[Constants.OKEX_FUTURE_GET_ORDER_WS] = self.update_get_order

    # 在update方法中会调用这个方法来删除register_callbacks注册的回调
    def remove_callbacks(self):
        del self.methods["ok_sub_future_btc_depth_this_week_usd"]
        del self.methods[Constants.OKEX_FUTURE_TRADE_WS]
        del self.methods[Constants.OKEX_FUTURE_DELETE_ORDER_WS]
        del self.methods[Constants.OKEX_FUTURE_GET_ORDER_WS]

    '''
    ? depth在比特币市场中是个什么概念？
    '''
    def get_depth(self):
        return self.depth

    '''
    这个方法会在register_clallbacks中注册为一个回调方法
    self.methods["ok_sub_future_btc_depth_this_week_usd"] = self.update_depth

    看下面的代码实现当收到ok_sub_future_btc_depth_this_week_usd这个channel的时候
    获取ask、bid值，并且存储到depth变量中
    并且将update_flags['depth']更新为True

    要理解这些方法的背景，就需要去阅读okex市场的接口文档了！
    '''
    async def update_depth(self, depth_data):
        list_of_ask = self.okex_depth_format(depth_data[0]["data"], "asks")
        list_of_bid = self.okex_depth_format(depth_data[0]["data"], "bids")
        self.depth = {"asks": list_of_ask, 'bids': list_of_bid}
        self.update_flags["depth"] = True

    '''
    这个方法会在register_clallbacks中注册为一个回调方法
    self.methods[Constants.OKEX_FUTURE_TRADE_WS] = self.update_trade
    self.methods[Constants.OKEX_FUTURE_DELETE_ORDER_WS] = self.update_trade

    看下面的代码实现当收到Constants.OKEX_FUTURE_TRADE_WS、Constants.OKEX_FUTURE_DELETE_ORDER_WS这个channel的时候
    获取值，并且存储到trade变量中
    并且将update_flags['trade']更新为True

    要理解这些方法的背景，就需要去阅读okex市场的接口文档了！
    '''
    async def update_trade(self, trade_data):
        '''
        :param trade_data: [{'data': {'result': True, 'order_id': 14420556515}, 'channel': 'ok_futuresusd_trade'}]
        :return: {'result': True, 'order_id': 14420556515}
        '''
        self.trade = trade_data[0]["data"]
        self.update_flags['trade'] = True

    '''
    这个方法会在register_clallbacks中注册为一个回调方法
    self.methods[Constants.OKEX_FUTURE_GET_ORDER_WS] = self.update_get_order    

    看下面的代码实现当收到Constants.OKEX_FUTURE_GET_ORDER_WS这个channel的时候
    获取值，并且存储到order变量中
    并且将update_flags['order']更新为True

    要理解这些方法的背景，就需要去阅读okex市场的接口文档了！
    '''
    async def update_get_order(self, order_data):
        '''
        :param order_data: [{'data': {'result': True, 'orders': [{'symbol': 'eth_usd', 'lever_rate': 10.0, 'amount': 1.0, 'fee': 0.0, 'contract_name': 'ETH1201', 'unit_amount': 10.0, 'type': 2, 'price_avg': 0.0, 'deal_amount': 0.0, 'price': 450.0, 'create_date': 1512029722000, 'order_id': 14495541683, 'status': -1}]}, 'channel': 'ok_futureusd_orderinfo'}]
        :return: {'result': True, 'orders': [{'symbol': 'eth_usd', 'lever_rate': 10.0, 'amount': 1.0, 'fee': 0.0, 'contract_name': 'ETH1201', 'unit_amount': 10.0, 'type': 2, 'price_avg': 0.0, 'deal_amount': 0.0, 'price': 450.0, 'create_date': 1512029722000, 'order_id': 14495541683, 'status': -1}]}
        '''
        self.order = order_data[0]["data"]
        self.update_flags['get_order'] = True

    '''
    先将所有的标志flags设置为Flase
    然后注册回调
    在一个循环中持续check_flags，即检查所有的标志是不是都变成True了
    然后移除刚才注册的回调
    '''
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

    '''
    调用trade_message()获取一个long的请求包
    将请求包放到队列中
    然后long()方法中调用update()持续等待（里面持续判断所有flag的值）
    如果判断所有flag为True，那么相当于知道收到应答了
    这时候就可以返回了
    因为在异步收到应答的时候已经调用对应的回调方法将trade的值更新了

    下面的short、close_long、close_short、delete_order都是类似的
    '''
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

    async def send(self):
        while not self.q.empty():
            message = self.q.get()
            await self.websocket.send(message)

    '''
    按照交易所的接口打包long、short、等的接口
    '''
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

    '''
    初始化的时候订阅频道
    '''
    async def ws_init(self):
        await self.sub_channel()



    '''
    这个和processing是重复的
    不过processing方法只是定义，但没有用到
    所以这里可以参考我在processing方法中的注释说明
    '''
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

    '''
    线程方法这种会调用eventloop来进行循环
    '''
    def run(self):
        asyncio.set_event_loop(self.loop)
        self.loop.run_until_complete(self.keep_connect())
        self.loop.run_until_complete(self.ws_init())
        self.loop.run_until_complete(self.ws_handler())
