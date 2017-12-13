import collections
import logging
from configparser import ConfigParser
from functools import partial
from pprint import pprint

import os

from dquant.constants import Constants
from dquant.util import Util

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

section_names = 'fortest', 'datadog', 'influxdb', 'mongo', 'okex','bitmex'


class MyConfiguration():
    __config_dict = collections.defaultdict(dict)

    def __init__(self, *file_names):
        parser = ConfigParser()
        parser.optionxform = str  # make option names case sensitive

        for file_name in file_names:
            found = parser.read(file_name)
            raw_file_name = result = file_name.split('/')[-1]
            group = Util.slice_till_dot(raw_file_name)

            if not found:
                raise ValueError('No config file found!')
            for name in section_names:
                self.__config_dict[group].update(parser.items(name))

    def pretty_print(self):
        pprint(self.__config_dict)

    def get_config_base(self, state, key):
        try:
            result = self.__config_dict[state][key]
            logging.info("key={}, result={}".format(key, result))

            return result
        except KeyError:
            logging.ERROR(KeyError)

    def get_config(self, key):
        return self.get_config_base(os.environ.get(Constants.DQUANT_ENV), key)

    def get_float_config(self, key):
        return  float(self.get_config(key))


cfg = MyConfiguration(os.path.join(os.path.dirname(__file__), '../config/dev.cfg'),
                      os.path.join(os.path.dirname(__file__), '../config/pro.cfg'))
