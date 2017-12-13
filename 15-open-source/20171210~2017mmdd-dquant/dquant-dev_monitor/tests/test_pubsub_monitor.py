import json
import unittest

import os

from dquant.constants import Constants
from dquant.pipeline.monitor_subscriber import monitorSubscriber
from dquant.pipeline.publisher import Publisher
'''
测试发布者、订阅者监控模块
'''


class ConfigTestSuite(unittest.TestCase):
    """for config test case"""

    @classmethod
    def setUpClass(cls):
        os.environ[Constants.DQUANT_ENV] = "dev"

    def test_json_dump(self):
        print(json.dumps({'symbol': 'eth',
                          'exchange': 'okex',
                          'price': '2999.31'}))

    def test_get_config(self):
        print(os.environ[Constants.DQUANT_ENV])

        # 订阅者订阅价格信息
        # class monitorSubscriber(threading.Thread):，订阅者是一个线程类
        sub = monitorSubscriber(['price'])
        # 启动线程
        sub.start()
        # 创建一个发布者
        publisher = Publisher()
        # 发布价格信息
        publisher.publish_price({'symbol': 'eth',
                                 'exchange': 'okex',
                                 'price': '2999.31'})
        # publisher = Publisher()
        # publisher.redis.publish('price',"simple message")


if __name__ == '__main__':
    unittest.main()
