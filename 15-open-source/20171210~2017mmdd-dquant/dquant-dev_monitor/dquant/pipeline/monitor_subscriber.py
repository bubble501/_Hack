import json

import redis
import threading

from dquant.config import cfg
from dquant.constants import Constants

'''
订阅者模块，也是通过redis来实现的
发布者将内容发布到redis，然后redis会推送给对应的订阅者
'''
class monitorSubscriber(threading.Thread):
    '''
    初始化的例子是
    sub = monitorSubscriber(['price'])
    '''
    def __init__(self, channels):
        threading.Thread.__init__(self)
        self.host = cfg.get_config(Constants.REDIS_HOST)
        self.port = cfg.get_config(Constants.REDIS_PORT)
        # 连接到redis
        self.redis = redis.Redis(host=self.host, port=self.port)
        # 创建一个订阅者发布者对象
        self.pubsub = self.redis.pubsub()
        # channels是一个list，里面存储所有需要订阅的频道
        self.pubsub.subscribe(channels)
        self.methods = {'price' : self.price_handler}

    def dispatcher(self, item):
        # 获取频道名称
        channel = item['channel'].decode("utf-8")
        # 获取该频道收到的订阅数据
        rawdata = item['data']
        if not isinstance(rawdata, int):
            # 对订阅数据进行json解析处理！
            data = json.loads(rawdata.decode("utf-8"))
            print(channel + ":" + json.dumps(data))
            '''
            methods是一个字典，存储的格式是{频道名称: 频道对应的处理方法}
            当收到某个频道后，可以根据methods快速的回调对应的频道处理方法！

            这是一个简单的事件驱动引擎
            '''
            self.methods[channel](data)

    def price_handler(self, data):
        print("In price handler")
        print("do whatever with data")

    '''
    monitorSubscriber是一个订阅者，以线程的形式运行
    始终调用listen()来监听是否有推送过来的订阅信息
    收到订阅信息后，调用dispatcher进行分发！
    '''
    def run(self):
        for item in self.pubsub.listen():
            print(item)
            self.dispatcher(item)
