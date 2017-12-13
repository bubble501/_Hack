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


if __name__ == '__main__':
    unittest.main()
