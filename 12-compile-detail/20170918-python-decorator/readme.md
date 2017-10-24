目前我的主力使用和学习的编程语言是Delphi、C、C++、Python、Lisp、ASM

但是说实话，不管是学习Delphi，还是学习C++，还是Python都依然停留在C的思维上，使用起来就是最简单的类、函数、条件、循环，这些语言的更多独有的特性、高效的编程方式完全没有很好的学习和使用，这是自己学习编程语言本身的一大局限

下面列出一些典型的自己学习学习其他非C的问题

* Delphi的控件部分是Delphi的一大核心，这块我熟悉度很差
* C++的模板、STL、智能指针、function、bind
* Python的装饰器、lambda等诸多特性都存在问题
* 等等

存在三个层面的问题：

* 首先，不清楚其语法
* 其次，更不熟悉其使用场景
* 最后，完全不知道其实现层面的机制

了解一些语言的特性最直接的好处就是在阅读一些好的开源项目的时候，可以比较流畅，比如muduo里面大量用到C++智能指针、function、bind；Flask中的Python装饰器、lambda等

今天对Python的装饰器进行一个简单的梳理

## 引子

先来个形象比方：内裤可以用来遮羞，但到了冬天它没法为我们防风御寒，聪明的人们发明了长裤，有了长裤之后就不再冷了，装饰器就像我们这里说的长裤，在不影响内裤的前提下，给我们的身子提供了保暖的功效

再回到主题

装饰器本质上是一个Python函数，它可以让其他函数在不需要做任何代码变动的前提下增加额外功能，装饰器的返回值也是一个函数对象。它经常用于 有切面需求的场景，比如：插入日志、性能测试、事务处理、缓存、权限校验等场景。装饰器是解决这类问题的绝佳设计，有了装饰器，我们就可以抽离出大量与函数本身无关的雷同代码并继续重用

>概括地讲，装饰器的作用就是为已经存在的对象添加额外的功能

先来看一个简单的例子

```
def foo():
    print('i am foo')
```

现在有一个新的需求，希望可以记录下函数的执行日志，于是在代码中添加日志代码：

```
def foo():
    print('i am foo')
    logging.info('foo is running')
```

bar()、bar2()也有类似的需求，怎么做？再写一个logging在bar函数里？这样就造成了大量雷同的代码。为了减少重写代码，我们可以这样做，重新定义一个函数：专门处理日志，日志处理完之后再写真正的业务代码

```
def use_logging(func):
    logging.warn("%s is running" % func.__name__)
    func()

def bar():
    print('i am bar')

use_logging(bar)
```

逻辑上不难理解，但是这样的话，我们每次都要将一个函数作为参数传递给use_logging函数。而且这种方式已经破坏了原有的代码逻辑结构，之前执行业务逻辑时，执行运行bar()，但现在不得不改成use_logging(bar)。那么有没有更好的方式呢？

当然有，答案就是装饰器

## 简单装饰器

```
def use_logging(func):
    def wrapper(*args, **kwargs):
        logging.warn('%s is running' % func.__name__)
        return func(*args, **kwargs)
    return wrapper

def bar():
    print('i am bar')

bar = use_logging(bar)
bar()
```

函数use_logging就是装饰器，它把执行真正业务方法的func包裹在函数里面，看起来像bar被use_logging装饰了

>在这个例子中，函数进入和退出时，被称为一个**横切面(Aspect)**，这种编程方式被称为面向切面的编程(Aspect-Oriented Programming)

## Python装饰器

@符号是装饰器的语糖，在定义函数的时候使用，避免再一次赋值操作

```
def use_logging(func):
    def wrapper(*args, **kwargs):
        logging.warn('%s is running' % func.__name__)
        return func(*args)
    return wrapper

@use_logging
def bar():
    print('i am bar')

bar()
```

如上所示，这样我们就可以省去`bar = use_logging(bar)这一句了，直接调用bar()即可得到想要的结果。如果我们有其他类似的函数，我们可以继续调用装饰器来修饰函数，而不用重复修改函数或者增加新的封装。这样，我们就提高了程序的可重复利用性，并增加了程序的可读性

>装饰器在Python使用如此方便都要归因于Python的函数能像普通的对象一样能作为参数传递给其他函数、可以被赋值给其他变量、可以作为返回值、可以被定义在另外一个函数内部

## 带参数的装饰器

装饰器还有更大的灵活性，例如带参数的装饰器：在上面的装饰器调用中，比如`@use_logging`，该装饰器唯一的参数就是执行业务的函数。装饰器的语法允许我们在调用时，提供其他参数，比如`@decorator(a)`。这样，就为装饰七的编写和使用提供了更大的灵活性

```
def use_logging(level):
    def decorator(func):
        def wrapper(*args, **kwargs):
            if level == 'warn':
                logging.warn('%s is running' % func.__name__)
            return func(*args)
        return wrapper
    return decorator

@use_logging(level = 'warn')
def foo(name = 'foo'):
    print('i am %s' % name)

foo()
```

上面的use_logging是允许带参数的装饰器。它实际上是对原有装饰器的一个函数封装，并返回一个装饰器。我们可以将它理解为一个含有参数的闭包。当我们使用`@use_logging(level='warn')`调用的时候，Python能够发现这一层的封装，并把参数传递到装饰器的环境中

## 类装饰器

相比于函数装饰器，类装饰器具有灵活性大、高内聚、封装性等优点。使用类装饰器还可以依靠内部的\_\_call\_\_方法，当使用@形式将装饰器附加到函数上时，就会调用此方法

```
class Foo(object):
    def __init__(self, func):
        self._func = func
    def __call__(self):
        print ('class decorator running')
        self._func()
        print ('class decorator ending')

@Foo
def bar():
    print ('bar')

bar()
```

## functools.wraps

使用装饰器极大地复用了代码，但它有一个缺点就是原函数的元信息不见了，比如函数docstring、\_\_name\_\_、参数列表，先看例子：

装饰器

```
def logged(func):
    def with_logging(&args, **kwargs):
        print func.__name__ + 'was called'
        return func(*args, **kwargs)
    return with_logging
```

函数

```
@logged
def f(x):
    """does some math"""
    return x + x * x
```

该函数完全等价于

```
def f(x):
    """does some math"""
    return x + x * x

f = logged(f)
```

不难发现，函数f被with_logging取代了，当然它的docstring、\_\_name\_\_就变成了with_logging的信息了

```
print f.__name__      # 'with_logging'
print f.__doc__       # None
```

这个问题就比较严重了，好在我们有functools.wraps，wraps本身也是一个装饰器，它能把原函数的元信息拷贝到装饰器函数中，这使得装饰器函数也有和原函数一样的元信息了

```
from functools import wraps
def logged(func):
    @wraps(func)
    def with_logging(*args, **kwargs):
        print func.__name__ + "was called"
        return func(*args, **kwargs)
    return with_logging

@logged
def f(x):
    """does some math"""
    return x + x * x

print f.__name__      # 'f'
print f__doc__        # 'does some math'
```

## 内置装饰器

@staticmethod

@classmethod

@property

## 装饰器的顺序

```
@a
@b
@c
def f():
```

等效于

```
f = a(b(c(f)))
```

## 参考资料

* [如何理解Python装饰器？](https://www.zhihu.com/question/26930016)