import unittest

import os

from dquant.config import cfg
from dquant.constants import Constants


class ConfigTestSuite(unittest.TestCase):
    """for config test case"""

    # setUpClass是用于在单元测试运行之前先执行的方法
    # 比如用来做一些必要的初始化工作
    @classmethod
    def setUpClass(cls):
        os.environ[Constants.DQUANT_ENV] = "dev"

    # 测试配置的解析功能
    @unittest.skip("only for debug")
    def test_config_parse(self):
        val = cfg
        val.pretty_print()
        assert True

    # 测试
    def test_get_config(self):
        val = cfg.get_config("test")
        self.assertTrue(val == "dev")

    # 测试获取redis的IP、端口信息
    def test_get_redis(self):
        host = cfg.get_config(Constants.REDIS_PORT)
        port = cfg.get_config(Constants.REDIS_HOST)
        print(host + port)

if __name__ == '__main__':
    unittest.main()
