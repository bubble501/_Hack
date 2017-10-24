# -*- coding: utf-8 -*-

from ctypes import *
import os

if __name__ == '__main__':
    libtest = cdll.LoadLibrary(os.getcwd() + '/libtest.so')
    print libtest.multiply(2, 3)
