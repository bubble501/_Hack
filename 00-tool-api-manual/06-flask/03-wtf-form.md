>详细讲述如何使用Web表单。为了在Flask中使用表单，将使用[Flask-WTF](http://packages.python.org/Flask-WTF)，该扩展封装了WTForms，并且恰当地集成进Flask中

## 配置文件

根目录下创建config.py用做配置文件

```
# CSRF_ENABLED为了激活“跨站点请求伪造”保护，使程序更安全
CSRF_ENABLED = True

# CSRF_ENABLED有效时才使用，建立一个加密的令牌，用于验证表单
SECRET_KEY = 'you-will-never-guess'
```

然后在`app/__init__.py`中读取配置文件

```
from flask import Flask

app = Flask(__name__)
app.config.from_object('config')

from app import views
```

## 登录表单

在Flask-WTF中，表单表示成对象，Form类的子类。一个表单子类简单地把表单的域定义成类的变量

编写一个表单文件`app/forms.py`

```
from flask.ext.wtf import Form
from wtforms import StringField, BooleanField

# DataRequired验证器只是简单地检查相应域提交的数据是否为空
# 在Flask-WTF中有许多的验证器，以后会看到它们
from wtforms.validators import DataRequired

class LoginForm(Form):
    # 这里选择的登录机制不是标准的用户名/密码，而是使用OpenID
    # OpenID的好处就是认证是由OpenID的提供者完成的
    # 因此我们不需要验证密码，这会让我们的网站对用户而言更加安全
    openid = StringField('openid', validators=[DataRequired()])

    # 提供remember me选项，以至于用户可以选择在网页浏览器上种植cookie
    # 当他们再次访问的时候，浏览器能够记住他们的登录
    remember_me = BooleanField('remember_me', default=False)
```

## 表单模板

我们同样需要一个包含生成表单的HTML的模板。好消息是我们刚刚创建的LoginForm类知道如何呈现为HTML表单字段，所以我们只需要集中精力到布局上。`app/templates/login.html`

```
<!-- extend from base layout -->
{% extends "base.html" %}

{% block content %}
<h1>Sign In</h1>
<form action = "" method="post" name="login">
  <!-- form.hidden_tag()模板参数将被替换为一个隐藏字段，
       用来实现在配置中激活的CSRF保护
       如果已经激活CSRF，该字段需要出现在所有的表单中 -->
  {{form.hidden_tag()}}
  <p>
    Please enter your OpenID:<br>
    <!-- 表单中实际的字段也将会被表单对象渲染
         你只需要在字段应该被插入的地方指明一个{{form.field_name}}模板参数 -->
    {{form.openid(size=80)}}<br>
    <!-- 循环获取验证openid字段的信息
         通常情况下，任何需要验证的字段都会把错误信息放入form.field_name.errors下 -->
    {% for error in form.openid.errors %}
      <span style='color:red;'>[{{ error }}]</span>
  </p>
  <p>{{form.remember_me}} Remember Me</p>
  <p><input type="submit" value="Sign In"></p>
</form>
{% endblock %}
```

## 表单视图

最后一步就是编写**渲染模板**的视图函数的代码。实际上这是十分简单的，因为我们只需要把一个表单对象传入模板中。`app/views.py`

```
from flask import render_template, flash, redirect
from app import app
from .forms import LoginForm

# index view function suppressed for brevity

@app.route('/login', method = ['GET', 'POST'])
def login():
    form = LoginForm()
    # 验证并存储表单数据，validate_on_submit方法做了所有表单处理工作
    # 当表单正在展示给用户时调用它，返回False
    # 如果在表单提交请求时被调用，它会收集所有数据，对字段进行验证
    # 如果所有的事情都通过的话，将会返回True，表示数据都是合法的
    # 如果至少一个字段验证失败的话，将会返回False，接着表单会重新呈现给用户
    if form.validate_on_submit():
        flash('Login requested for OpenID="' + form.openid.data + '", remember_me=' + str(form.remember_me.data))
        return redirect('/index')
    return render_template('login.html', title = 'Sign In', form = form)
```

>视图函数中将LoginForm对象传给HTML模板文件，用做渲染！

闪现的消息将不会自动出现在页面上，我们的模板需要加入展示消息的内容。我们将添加这些消息到基础模板中，这样所有的模板都能继承这个函数。`app/templates/base.html`

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
    <div>Microblog: <a href="/index">Home</a></div>
    <hr>
    {% with messages = get_flashed_messages() %}
    {% if messages %}
    <ul>
    {% for message in messages %}
        <li>{{ message }} </li>
    {% endfor %}
    </ul>
    {% endif %}
    {% endwith %}
    {% block content %}{% endblock %}
  </body>
</html>
```

## 扩展

事实上，很多用户并不知道他们已经有一些OpenIDs。一些大的互联网服务提供商支持OpenID认证自己的会员这并不是众所周知的。比如，如果你有一个Google的账号，你也就有了一个它们的OpenID

OpenIDs在[《处理 OpenIDs》](http://www.pythondoc.com/flask-mega-tutorial/webforms.html#openids)这一小节有比较详细的介绍

>关于OpenIDs、如何在真实的项目中对用户数据进行验证保证系统的安全将会是一个更大的话题