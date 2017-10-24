之前已经参考[《Flask大型教程项目》](http://www.pythondoc.com/flask-mega-tutorial/)分别整理了以下的内容

* [《Hello World》](https://github.com/HackerLaboratory/_Crack/blob/master/20170815~2017mmdd-tool-api-manual/06-flask/01-hello-world.md)
* [《模板》](https://github.com/HackerLaboratory/_Crack/blob/master/20170815~2017mmdd-tool-api-manual/06-flask/02-jinja2-template.md)
* [《Web表单》](https://github.com/HackerLaboratory/_Crack/blob/master/20170815~2017mmdd-tool-api-manual/06-flask/03-wtf-form.md)

发现如果继续这样学习的话，没有任何卵用，还是直接上手去写代码，写代码的过程中如果有什么特别需要注意的技巧可以再整理到这里

而不应该是直接无脑的将一份开发的教程，做文档化的整理，而不写代码！必须要实际的写代码才行。尤其是现在的我总是进行大量的“死学习”，不做实际的动手操作！缺的就是动手编码的经验，而不是空想、空学

>以上这一点必须再再再次强调，软件开发是一个实践性的领域，必须要大量的动手，在实践中总结经验、在实践中学习，而不是像以前应试教育那种学法。必须尽快转变自己的思维模式、学习习惯！！！！！！！

>必须他妈的动手！必须他妈的动手！必须他妈的动手！

>错误的学习、实践方法浪费了老子多少的时间！而且收到的是事倍功半的恶果！！

---

接下来的内容根据在开发中的实际情况，选择性的只罗列在这里，但后续不在向上面三个部分那样做详细整理：

* [《数据库》](http://www.pythondoc.com/flask-mega-tutorial/database.html)
* [《用户登录》](http://www.pythondoc.com/flask-mega-tutorial/userlogin.html)
* [《用户信息页和头像》](http://www.pythondoc.com/flask-mega-tutorial/profile.html)
* [《单元测试》](http://www.pythondoc.com/flask-mega-tutorial/testing.html)
* [《关注者，联系人和好友》](http://www.pythondoc.com/flask-mega-tutorial/followers.html)
* [《分页》](http://www.pythondoc.com/flask-mega-tutorial/pagination.html)
* [《全文搜索》](http://www.pythondoc.com/flask-mega-tutorial/textsearch.html)
* [《邮件支持》](http://www.pythondoc.com/flask-mega-tutorial/email.html)
* [《换装》](http://www.pythondoc.com/flask-mega-tutorial/facelift.html)
* [《日期和时间》](http://www.pythondoc.com/flask-mega-tutorial/dateandtime.html)
* [《国际化和本地化》](http://www.pythondoc.com/flask-mega-tutorial/i18n.html)
* [《Ajax》](http://www.pythondoc.com/flask-mega-tutorial/ajax.html)
* [《调试，测试以及优化》](http://www.pythondoc.com/flask-mega-tutorial/debugging.html)

>接下来计划参考《Flask大型教程项目》《Flask Web开发》，直接先在[_Gamble](https://github.com/HackerLaboratory/_Laboratory/tree/master/_Project/_Gamble)模仿完成一个简单的Web项目，在此期间的重点是在代码中对Flask、Python的用法进行充分的注解！保证这次能够从各个细节之处**解构**Python Flask Web开发，为后续从大层面架构系统做充分准备

>模仿完成一个项目之后，在此基础上结合在自己的需求进行修改。包括：开发爬虫、更改系统主题、完善架构、性能调优……

## [《数据库》](http://www.pythondoc.com/flask-mega-tutorial/database.html)

使用[Flask-SQLAlchemy](http://packages.python.org/Flask-SQLAlchemy)扩展来管理我们应用程序的数据。这个扩展封装了[SQLAlchemy](http://www.sqlalchemy.org/)项目，这是一个[对象关系映射器](http://en.wikipedia.org/wiki/Object-relational_mapping)或者ORM

ORMs允许数据库应用程序与对象一起工作，而不是表以及SQL。执行再对象的操作会被ORM翻译成数据库命令。这就意味着我们将不需要学习SQL，将使用Flask-SQLAlchemy代替SQL

我们存储在数据库中的数据将会以类的集合来表示，我们称之为数据库模型。ORM层需要做的翻译就是将从这些类创建的对象映射到适合的数据库表的行

```
from app import db

class User(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    nickname = db.Column(db.String(64), index=True, unique=True)
    email = db.Column(db.String(120), index=True, unique=True)
    # User类增加新字段Post，被构建成一个db.relationship字段
    # 这并不是一个实际的数据库字段，因此不会出现在数据表中
    # 对于一个一对多的关系，db.relationship字段通常是定义在“一”这一边
    # 这种关系下，我们得到一个user.posts成员，它给出一个用户所有的blog
    posts = db.relationship('Post', backref='author', lazy='dynamic')
    
    # __repr__方便在Python中打印对象信息
    def __repr__(self):
        return '<User %r>' % (self.nickname)

# Post类表示拥护编写的blog
class Post(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    body = db.column(db.String(140))
    # user_id初始化为外键，因此Flask-SQLAlchemy知道这个字段是连接到用户上
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'))

    def __repr__(self):
        return '<Post %r>' % (self.body)
```

>那就存在一个疑问了！无法直接使用SQL，那么在做数据库性能调优的时候如何针对SQL做调优？

## [《用户登录》](http://www.pythondoc.com/flask-mega-tutorial/userlogin.html)

有了表单和数据库后，需要建立起表单和数据库的联系，实现用户的登入和等出

## [《用户信息页和头像》](http://www.pythondoc.com/flask-mega-tutorial/profile.html)



## [《单元测试》](http://www.pythondoc.com/flask-mega-tutorial/testing.html)



## [《关注者，联系人和好友》](http://www.pythondoc.com/flask-mega-tutorial/followers.html)



## [《分页》](http://www.pythondoc.com/flask-mega-tutorial/pagination.html)



## [《全文搜索》](http://www.pythondoc.com/flask-mega-tutorial/textsearch.html)



## [《邮件支持》](http://www.pythondoc.com/flask-mega-tutorial/email.html)



## [《换装》](http://www.pythondoc.com/flask-mega-tutorial/facelift.html)



## [《日期和时间》](http://www.pythondoc.com/flask-mega-tutorial/dateandtime.html)



## [《国际化和本地化》](http://www.pythondoc.com/flask-mega-tutorial/i18n.html)



## [《Ajax》](http://www.pythondoc.com/flask-mega-tutorial/ajax.html)



## [《调试，测试以及优化》](http://www.pythondoc.com/flask-mega-tutorial/debugging.html)

