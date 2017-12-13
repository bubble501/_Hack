import json
import logging

import aiohttp
from pymongo import MongoClient

import datetime, time
import requests, hashlib
from requests import RequestException
from urllib.parse import urlencode

from dquant.config import cfg
from dquant.constants import Constants
from dquant.markets.market import Market


class OkexFutureRest(Market):
    def __init__(self, meta_code):
        base_currency, market_currency, symbol, contract_type = self.parse_meta(meta_code)
        super().__init__(base_currency, market_currency, meta_code, cfg.get_float_config(Constants.OKEX_FEE))
        self.apikey = cfg.get_config(Constants.OKEX_APIKEY)
        self.apisec = cfg.get_config(Constants.OKEX_APISEC)
        self.contract_type = contract_type
        self.symbol = symbol
        self.base_url = Constants.OKEX_FUTURE_REST_BASE
        self.timeout = Constants.OK_HTTP_TIMEOUT

    async def get_ticker(self):
        depth = await self.get_depth()
        if not depth:
            return None
        res = {'ask': {'price': 0, 'amount': 0}, 'bid': {'price': 0, 'amount': 0}}
        if len(depth['asks']) > 0:
            res['ask'] = depth['asks'][0]
        if len(depth["bids"]) > 0:
            res['bid'] = depth['bids'][0]
        return res

    # 轮询获取depth
    async def get_depth(self):
        params = {"symbol": self.symbol, "contract_type": self.contract_type}
        while True:
            try:
                res = await self.request(Constants.OKEX_FUTURE_DEPTH_RESOURCE_REST, params, "get")
                list_of_ask = self.okex_depth_format(res, "asks")
                list_of_ask.reverse()
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


    async def long(self, amount, price='', lever_rate='10'):
        res = await self.okex_request(price=price, amount=amount, type='1', api_url=Constants.OKEX_FUTURE_TRADE_REST)
        return res

    async def short(self, amount, price='', lever_rate='10'):
        res = await self.okex_request(price=price, amount=amount, type='2', api_url=Constants.OKEX_FUTURE_TRADE_REST)
        return res

    async def close_long(self, amount, price=''):
        res = await self.okex_request(price=price, amount=amount, type='3', api_url=Constants.OKEX_FUTURE_TRADE_REST)
        return res

    async def close_short(self, amount, price=''):
        res = await self.okex_request(price=price, amount=amount, type='4', api_url=Constants.OKEX_FUTURE_TRADE_REST)
        return res

    async def delete_order(self, order_id, tillOK=True):
        '''
        :param order_id:
        :return: {'result': True, 'order_id': '14435081666'}
        '''
        while True:
            try:
                res = await self.okex_request(order_id=order_id, api_url=Constants.OKEX_FUTURE_DELETE_ORDER_REST)
                if 'result' in res:
                    if res['result'] == True:
                        return res
                if tillOK ==True:
                    continue
                else:
                    logging.error(res)
                    break
            except Exception as ex:
                logging.exception("message")
                if tillOK:
                    continue
                return None

    async def get_account(self, coin=[]):
        '''
        :return:{"info": {"btc": {"account_rights": 1,"keep_deposit": 0,"profit_real": 3.33,"profit_unreal": 0,"risk_rate": 10000},"ltc": {"account_rights": 2,"keep_deposit": 2.22,"profit_real": 3.33,"profit_unreal": 2,"risk_rate": 10000},"result": true}
        '''
        res = await self.okex_request(api_url=Constants.OKEX_FUTURE_USERINFO_REST)
        if res['result'] is True:
            if coin:
                ret = {}
                for c in coin:
                    if c.lower() in res["info"]:
                        ret[c.upper()] = res["info"][c.lower()]['balance']
                    else:
                        ret[c.upper()] = 0.0
                return ret
            else:
                return res["info"]

    # 向API发送请求
    async def okex_request(self, api_url, **kwargs):
        '''
        :param price: 默认对手价
        :param amount: 最小为1
        :param type: 1:开多 2:开空 3:平多 4:平空, 'delete_order':取消订单
        :param lever_rate: 杠杆倍数 value:10\20 默认10
        :param match_price: 是否为对手价 0:不是  1:是,当取值为1时,price无效。这里根据price是否为空判断。
        :param contract_type: 合约类型: this_week:当周   next_week:下周   quarter:季度
        :return: {"order_id":986,"result":true}
        '''
        params = {}

        if api_url is Constants.OKEX_FUTURE_DELETE_ORDER_REST:
            order_id = kwargs.get('order_id', None)
            params = {'api_key': self.apikey, 'symbol': self.symbol, 'contract_type': self.contract_type, 'order_id': order_id}

        elif api_url is Constants.OKEX_FUTURE_TRADE_REST:
            params = {'api_key': self.apikey, 'symbol': self.symbol, 'contract_type': self.contract_type, 'amount': str(kwargs.get('amount')),
                      'type': str(kwargs.get('type')), 'match_price': '1', 'lever_rate': str(kwargs.get('lever_rate', 10))}
            price = kwargs.get('price', None)
            if price:
                params['price'] = str(price)
                params['match_price'] = '0'

        elif api_url is Constants.OKEX_FUTURE_USERINFO_REST:
            params = {'api_key': self.apikey}

        params['sign'] = self.buildMySign(params, self.apisec)
        res = await self.request(api_url, params=params, type='post')
        return res

    async def request(self, resource, params, type):
        headers = {
            "Content-type": "application/x-www-form-urlencoded",
        }
        async with aiohttp.ClientSession() as session:
            if type == "post":
                temp_params = urlencode(params)
                async with session.post(url=self.base_url + resource, data=temp_params, timeout=self.timeout, headers=headers) as res:
                    if res.headers['CONTENT-TYPE'] == 'application/json':
                        return await res.json()
                    else:
                        text = await res.text()
                        return json.loads(text)

            elif type == "get":
                async with session.get(url=self.base_url + resource, params=params, timeout=self.timeout) as res:
                    if res.headers['CONTENT-TYPE'] == 'application/json':
                        return await res.json()
                    else:
                        text = await res.text()
                        return json.loads(text)

    def buildMySign(self, params, secretKey):
        sign = ''
        for key in sorted(params.keys()):
            sign += key + '=' + str(params[key]) + '&'
        data = sign + 'secret_key=' + secretKey
        return hashlib.md5(data.encode("utf8")).hexdigest().upper()


    def parse_meta(self, meta_code):
        meta_table = {"btc_usd_this_week": ("btc", "usd", "btc_usd", "this_week"),
                      "btc_usd_next_week": ("btc", "usd", "btc_usd", "next_week"),
                      "btc_usd_quarter": ("btc", "usd", "btc_usd", "quarter"),
                      "eth_usd_this_week": ("eth", "usd", "eth_usd", "this_week"),}
        return meta_table[meta_code]
