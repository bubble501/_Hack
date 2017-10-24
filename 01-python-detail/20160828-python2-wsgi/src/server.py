# -*- coding:utf-8 -*-

#server.py负责启动WSGI服务器，加载application()函数

from wsgiref.simple_server import make_server
from hello import application

#创建一个服务器，处理函数为application
httpd = make_server('', 10000, application)
print('Serving HTTP on port 10000')

#开始监听HTTP请求
httpd.serve_forever()
