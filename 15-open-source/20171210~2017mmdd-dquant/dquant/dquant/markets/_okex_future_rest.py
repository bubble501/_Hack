import json
import logging

import requests, hashlib
from requests import RequestException
from urllib.parse import urlencode

from dquant.config import cfg
from dquant.constants import Constants
from dquant.markets.market import Market


# def okex_auth(inFunction):
#     def outFunction(*args):
#         params = args[1]
#         params["api_key"] = okex_future.apikey
#         params["sign"] = okex_future.sign(params)
#
#         return inFunction(args[0], params)
#
#     return outFunction


class OkexFutureRest(Market):
    def __init__(self, meta_code):
        base_currency, market_currency, symbol, contract_type = self.parse_meta(meta_code)
        super().__init__(base_currency, market_currency, meta_code, cfg.get_float_config(Constants.OKEX_FEE))
        self.apikey = cfg.get_config(Constants.OKEX_APIKEY)  # apikey
        self.apisec = cfg.get_config(Constants.OKEX_APISEC)  # apisec
        self.contract_type = contract_type                   # 合约类型？
        self.symbol = symbol                                 # ？？
        self.base_url = Constants.OKEX_FUTURE_REST_BASE      # okex市场的URL："https://www.okex.com"
        self.session = requests.session()                    # 通过request模块创建一个session
        self.timeout = Constants.OK_HTTP_TIMEOUT             # 超时时间：2000

    # 获取ticker（买一卖一报价）
    def get_ticker(self):
        depth = self.get_depth()
        if not depth:
            return None
        res = {'ask': {'price': 0, 'amount': 0}, 'bid': {'price': 0, 'amount': 0}}
        if len(depth['asks']) > 0:
            res['ask'] = depth['asks'][0]
        if len(depth["bids"]) > 0:
            res['bid'] = depth['bids'][0]
        return res

    # 轮询获取depth
    def get_depth(self):
        params = {"symbol": self.symbol,
                  "contract_type": self.contract_type}
        while True:
            try:
                res = self.request(Constants.OKEX_FUTURE_DEPTH_RESOURCE_REST, params, "get")
                list_of_ask = self.okex_depth_format(res, "asks")
                list_of_bid = self.okex_depth_format(res, "bids")

                return {"asks": list_of_ask, 'bids': list_of_bid}
            except Exception:
                logging.exception("http error")

    # 格式化depth数据
    def okex_depth_format(self, res, flag):
        result_list = []
        for ticker in res[flag]:
            result_list.append({
                'price': ticker[0],
                'amount': ticker[1]
            })
        return result_list

    def long(self, amount, price='', lever_rate='10'):
        res = self.trade(price=price, amount=amount, type='1')
        return res

    def short(self, amount, price='', lever_rate='10'):
        res = self.trade(price=price, amount=amount, type='2')
        return res

    def close_long(self, amount, price=''):
        res = self.trade(price=price, amount=amount, type='3')
        return res

    def close_short(self, amount, price=''):
        res = self.trade(price=price, amount=amount, type='4')
        return res

    def delete_order(self, order_id, tillOK=True):
        '''
        :param order_id:
        :return: {'result': True, 'order_id': '14435081666'}
        '''
        while True:
            try:
                res = self.trade(price=None, amount=None, type='delete_order', order_id=order_id)
                if 'result' in res:
                    if res['result'] == True:
                        return res
                if tillOK == True:
                    continue
                else:
                    logging.error(res)
                    break
            except Exception as ex:
                logging.exception("message")
                if tillOK:
                    continue
                return None

    def trade(self, price, amount, type, lever_rate='10', **args):
        '''
        :param price: 默认对手价
        :param amount: 最小为1
        :param type: 1:开多 2:开空 3:平多 4:平空, 'delete_order':取消订单
        :param lever_rate: 杠杆倍数 value:10\20 默认10
        :param match_price: 是否为对手价 0:不是  1:是,当取值为1时,price无效。这里根据price是否为空判断。
        :param contract_type: 合约类型: this_week:当周   next_week:下周   quarter:季度
        :return: {"order_id":986,"result":true}
        '''
        if type == 'delete_order':
            order_id = args.get('order_id')
            params = {'api_key': self.apikey, 'symbol': self.symbol, 'contract_type': self.contract_type,
                      'order_id': order_id}
            params['sign'] = self.buildMySign(params, self.apisec)
            # OKEX_FUTURE_DELETE_ORDER_REST = "/api/v1/future_cancel.do"
            res = self.request(Constants.OKEX_FUTURE_DELETE_ORDER_REST, params=params, type='post')
        else:
            match_price = '0' if price else '1'
            params = {'api_key': self.apikey, 'symbol': self.symbol, 'contract_type': self.contract_type,
                      'amount': str(amount),
                      'type': type, 'match_price': match_price, 'lever_rate': str(lever_rate)}
            if price:
                params['price'] = str(price)
            params['sign'] = self.buildMySign(params, self.apisec)
            # OKEX_FUTURE_TRADE_REST = "/api/v1/future_trade.do?"
            res = self.request(Constants.OKEX_FUTURE_TRADE_REST, params=params, type='post')

        return res

    # 向okex市场请求
    def request(self, resource, params, type):
        headers = {
            "Content-type": "application/x-www-form-urlencoded",
        }
        if type == "post":
            temp_params = urlencode(params)
            # self.session = requests.session()
            # 向okex平台发起post请求
            res = self.session.post(url=self.base_url + resource, data=temp_params, timeout=self.timeout,
                                    headers=headers)
        elif type == "get":
            res = self.session.get(url=self.base_url + resource, params=params, timeout=self.timeout)

        if res.status_code == 200:
            return json.loads(res.content, encoding='utf-8')
        else:
            logging.exception("request error")
            raise RequestException("status error")

    def buildMySign(self, params, secretKey):
        sign = ''
        for key in sorted(params.keys()):
            sign += key + '=' + str(params[key]) + '&'
        data = sign + 'secret_key=' + secretKey
        return hashlib.md5(data.encode("utf8")).hexdigest().upper()

    def parse_meta(self, meta_code):
        meta_table = {"btc_usd_this_week": ("btc", "usd", "btc_usd", "this_week"),
                      "eth_usd_this_week": ("eth", "usd", "eth_usd", "this_week"), }
        return meta_table[meta_code]
