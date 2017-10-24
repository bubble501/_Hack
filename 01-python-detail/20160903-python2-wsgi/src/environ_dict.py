# -*- coding: utf-8 -*-

from wsgiref.simple_server import make_server

def application(environ, start_response):
    
    response_body = ['%s: %s' % (key, value)
                    for key, value in sorted(environ.items())]
    response_body = '\n'.join(response_body)
    status = '200 OK'
    response__headers = [('Content-Type', 'text/plain'), 
                        ('Content-Length', str(len(response_body)))]
    start_response(status, response__headers);
    #在application对象返回时，用return [response_body]
    #用 return response_body同样可以工作，但是效率会很低
    #因为返回时回去迭代response字符串中的每一个字符
    #所以当处理response字符串时，最好是将它包在一个可迭代对象中，比如list
    return [response_body]

http = make_server('localhost', 8080, application)
http.handle_request()
