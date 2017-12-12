import unittest

import os

from dquant.config import cfg
from dquant.constants import Constants


class ConfigTestSuite(unittest.TestCase):
    """for config test case"""

    @classmethod
    def setUpClass(cls):
        os.environ[Constants.DQUANT_ENV] = "dev"

    @unittest.skip("only for debug")
    def test_config_parse(self):
        val = cfg
        val.pretty_print()
        assert True


    def test_get_config(self):
        val = cfg.get_config("test")
        self.assertTrue(val == "dev")


    def test_get_redis(self):
        host = cfg.get_config(Constants.REDIS_PORT)
        port = cfg.get_config(Constants.REDIS_HOST)
        print(host + port)

if __name__ == '__main__':
    unittest.main()
