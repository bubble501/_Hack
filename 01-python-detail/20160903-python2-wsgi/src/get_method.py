# -*- coding: utf-8 -*-

from wsgiref.simple_server import make_server
from cgi import parse_qs, escape

html = """
<html>
<body>
    <form method = "get" action = "/">
        <p>
            Name: <input type = "text" name = "name">
        </p>
        <p>
            Hobbies:
            <input name = "hobbies" type = "checkbox" value = "running"> running
            <input name = "hobbies" type = "checkbox" value = "swimming"> swimming
            <input name = "hobbies" type = "checkbox" value = "reading"> reading
        </p>
        <p>
            <input type = "submit" value = "Submit">
        </p>
    </form>
    <p>
        Name: %s<br>
        Hobbies: %s
    </p>
</body>
</html>"""

def application(environ, start_response):
    print "QUERY_STRING: %s" % environ['QUERY_STRING']
    print "REQUEST_METHOD: %s" % environ['REQUEST_METHOD']

    #parse_qs解析QUERY_STRING获得对应的字典
    d = parse_qs(environ['QUERY_STRING'])

    name = d.get('name', [''])[0]       #Return the first name value
    hobbies = d.get('hobbies', [])      #Return a list of hobbies

    #escape过滤输入，防止客户端的输入存在脚本注入
    name = escape(name)
    hobbies = [escape(hobby) for hobby in hobbies]  #循环处理hobbies链表中的每个元素

    response_body = html % (name or 'Empty', ','.join(hobbies or ['No Hobbies']))
    status = '200 OK'

    response_headers = [('Content-Type', 'text/html'), ('Content-Length', str(len(response_body)))]

    start_response(status, response_headers)

    return [response_body]

httpd = make_server('localhost', 8080, application)
httpd.serve_forever()
