import json

import redis

from dquant.config import cfg
from dquant.constants import Constants

'''
发布者模块是通过redis来实现的（需要连接到redis）
比如调用publish_price()方法
就会将价格信息，打包成json的格式，然后publish到redis中
'''
class Publisher():
    def __init__(self):
        self.host = cfg.get_config(Constants.REDIS_HOST)
        self.port = cfg.get_config(Constants.REDIS_PORT)
        self.redis = redis.Redis(host=self.host, port=self.port)

    '''
    发布信息调用的例子是
    publisher.publish_price({'symbol': 'eth',
                                 'exchange': 'okex',
                                 'price': '2999.31'})
    '''
    def publish_price(self,data):
        '''
        data should be json encodes
        :return:
        '''
        self.redis.publish('price',json.dumps(data))


