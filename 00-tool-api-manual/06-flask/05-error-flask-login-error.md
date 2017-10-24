使用下面的代码，运行程序报错

```
if g.user is not None and g.user.is_authenticated():
  ...
```

报错信息如下

```
Traceback (most recent call last):
  File "/Users/xumenger/Desktop/code/Laboratory/_LAB/lib/python2.7/site-packages/flask/app.py", line 1997, in __call__
    return self.wsgi_app(environ, start_response)
  File "/Users/xumenger/Desktop/code/Laboratory/_LAB/lib/python2.7/site-packages/flask/app.py", line 1985, in wsgi_app
    response = self.handle_exception(e)
  File "/Users/xumenger/Desktop/code/Laboratory/_LAB/lib/python2.7/site-packages/flask/app.py", line 1540, in handle_exception
    reraise(exc_type, exc_value, tb)
  File "/Users/xumenger/Desktop/code/Laboratory/_LAB/lib/python2.7/site-packages/flask/app.py", line 1982, in wsgi_app
    response = self.full_dispatch_request()
  File "/Users/xumenger/Desktop/code/Laboratory/_LAB/lib/python2.7/site-packages/flask/app.py", line 1614, in full_dispatch_request
    rv = self.handle_user_exception(e)
  File "/Users/xumenger/Desktop/code/Laboratory/_LAB/lib/python2.7/site-packages/flask/app.py", line 1517, in handle_user_exception
    reraise(exc_type, exc_value, tb)
  File "/Users/xumenger/Desktop/code/Laboratory/_LAB/lib/python2.7/site-packages/flask/app.py", line 1612, in full_dispatch_request
    rv = self.dispatch_request()
  File "/Users/xumenger/Desktop/code/Laboratory/_LAB/lib/python2.7/site-packages/flask/app.py", line 1598, in dispatch_request
    return self.view_functions[rule.endpoint](**req.view_args)
  File "/Users/xumenger/Desktop/code/Laboratory/_LAB/lib/python2.7/site-packages/flask_openid.py", line 500, in decorated
    return f(*args, **kwargs)
  File "/Users/xumenger/Desktop/github/HackerLaboratory/_Laboratory/_Project/_Gamble/website/sources/app/views.py", line 79, in login
    if g.user is not None and g.user.is_authenticated():
TypeError: 'bool' object is not callable
```

结果是因为Flask-Login的版本有问题，切换回旧版本就OK了

```
>pip uninstall Flask-Login
>pip install -v Flask-Login==0.2.11
```

从这一个点可以简单推理，还不知道Flask和Flask的各种扩展中还有多少的坑呢？所以使用Flask开发，后续进行充分的功能测试、安全测试、压力测试都是极其必要的
