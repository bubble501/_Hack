# -*- coding: utf-8 -*-

from wsgiref.simple_server import make_server
from cgi import parse_qs, escape

html = """
<html>
<body>
    <form method = "post" action = "parsing_post.wsgi">
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
    try:
        request_body_size = int(environ.get('CONTENT_LENGTH', 0))
    except (ValueError):
        request_body_size = 0

    request_body = environ['wsgi.input'].read(request_body_size)
    d = parse_qs(request_body)
    
    print "wsgi.input %s" % environ['wsgi.input']
    print "request_body_size %s" % environ.get('CONTENT_LENGTH', 0)
    print "request_body %s" % request_body

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
