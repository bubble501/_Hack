# -*- coding:utf-8 -*-

#这个application函数就是符合WSGI标准的HTTP处理函数，它接收两个参数
# environ       一个包含所有HTTP请求信息的dict对象
# start_response一个发送HTTP响应的函数
def application(environ, start_response):
    # start_response被调用就发送了HTTP响应的Header
    # 注意Ｈｅａｄｅｒ只能发送一次，也就是只能调用一次start_response
    # start_response函数接收两个参数，一个是HTTP响应码，一个是一组list表示HTTP Header，每个Header用一个包含两个str的tuple表示
    start_response('200 OK', [('Content-Type', 'text/html')])
    
    # 从environ中读取PATH_INFO，这样就可以显示更多动态的内容
    body = '<h1>Hello, %s!</h1>' % (environ['PATH_INFO'][1:] or 'web')

    # 函数的返回值将作为HTTP响应的body发送给浏览器
    return [body.encode('utf-8')]


#说明
# 有了WSGI，我们关心的就是如何从environ这个dict对象中拿到HTTP请求信息，然后构造HTML，通过start_response发送Header，最后返回Body
# 整个application函数本身没有涉及到任何解析HTTP的部分，也就是说，底层代码不需要我们自己编写，我们只负责在更高层次上考虑如何响应请求即可

# 不过，这个application函数怎么调用？如果我们自己调用，两个参数environ和start_response我们没有办法提供，返回的bytes也没有办法发给浏览器
# 所以application函数必须由WSGI服务器来调用
# 有很多符合WSGI规范的服务器，我们挑选一个来用，我们选择一个最简单的WSGI服务器，把这个简单的Web应用程序跑起来
# Python内置了一个WSGI服务器，模块叫wsgiref，它是用纯Python编写的WSGI服务器的参考实现
# 所谓参考实现，是指实现完全符合WSGI标准，但不考虑任何运行效率，仅供开发和测试使用
# 对应参考server.py
