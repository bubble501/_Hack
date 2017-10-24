按照“hello-world”的实践过程，现在的文件结构如下

```
microblog\
flask\
        <virtual environment files>
app\
        static\
        templates\
        __init__.py
        views.py
tmp\
run.py
```

可以执行`./run.py`来运行应用程序，然后在网页浏览器上打开`http://localhost:5000`网址访问

## 模板的使用

编写模板文件`app/templates/index.html`

```
<html>
  <head>
    <title>{{title}} - microblog</title>
  </head>
  <body>
    <h1>Hello, {{user.nickname}}!</h1>
  </body>
</html>
```

然后在视图函数`app/views.py`中使用这些模板

```
from flask import render_template
from app import app

@app.route('/')
@app.route('/index')
def index():
    user = {'nickname': 'Miguel'}
    return render_template("index.html", title = 'Home', user = user)
```

>`render_template`调用Jinja2模板引擎，Jinja2是Flask框架的一部分。Jinja2会把模板参数提供的相应的值替换了`{{...}}块`

## 模板中的控制语句

>Jinja2模板同样支持控制语句，像在`{%...%}`块中，可以使用if、for等控制语句

**条件语句**

```
<html>
  <head>
    {% if title %}
    <title>{{title}} - microblog</title>
    {% else %}
    <title>Welcome to microblog</title>
    {% endif %}
  </head>
  <body>
    <h1>Hello, {{user.nickname}}!</h1>
  </body>
</html>>
```

**循环语句**

比如在microblog应用程序中，登录的用户想要在首页展示他的或者她的联系人列表中用户最近的文章，因此让我们看看如何才能做到

首先我们创建一些用户以及他们的文章用来展示（文件app/views.py）

```
def index():
    user = {'nickname': 'Miguel'}
    posts = [
        {
            'author': {'nickname': 'John'},
            'body': 'Beautiful day in Portland !'
        },
        {
            'author': {'nickname': 'Susan'},
            'body': 'The Auengers movie wa so cool !'
        }
    ]
    
    return render_template("index.html", title = 'Home', user = user, posts = posts)
```

然后在模板中可以循环展示

```
<html>
  <head>
    {% if title %}
      <title>{{title}} - microblog</title>
    {% else %}
      <title>microblog</title>
    {% endif %}
  </head>
  <body>
    <h1>Hi, {{user.nickname}} !</h1>
    {% for post in posts %}
      <p>{{post.author.nickname}} says: <b>{{post.body}}</b></p>
    {% endfor %}
  </body>
</html>
```

另外模板还有继承的概念，具体在使用的时候可以专门查资料

>按照习惯，最后提一个问题供自己后续思考：使用模板来渲染HTML的性能怎么样，当有大量用户请求时，这一步是不是瓶颈？简单想想需要将模板加载到内存，然后将传入的数据填充到模板，那么这个过程的性能情况如何？如何测试？如何优化？
