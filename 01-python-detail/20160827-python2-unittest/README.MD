## 说明

本文执行`python2 -m unittest mydict_test`或`python3 -m unittest mydict_test`都OK

之前的几个练习都是用Python2实现的，本次则可以兼容Python2、Python3

**相关文章**

* 例子参考自[《单元测试》](http://www.liaoxuefeng.com/wiki/0014316089557264a6b348958f449949df42a6d3a2e542c000/00143191629979802b566644aa84656b50cd484ec4a7838000)
* 关于Python单元测试的详细整理，参见[《对Python程序进行单元测试》](http://www.xumenger.com/python-unittest-20160826/)

## 再简单的代码也要手动实现

就这么个简单的测试程序，实现的过程中还出现了各种bug，所以就算是再简单的代码也非得亲自动手实现，才能感性的认知运行结果，才能对理性的代码逻辑有感性的认知

**错误一**

```
def __init__(self, **kw):
    super(Dict, self).__init__(**kw)
```

错写成

```
def __init__(self, **kw):
    super().__init__(**kw)
```

当然了，这个是[《单元测试》](http://www.liaoxuefeng.com/wiki/0014316089557264a6b348958f449949df42a6d3a2e542c000/00143191629979802b566644aa84656b50cd484ec4a7838000)中给的例子本身就有的问题

**错误二**

```
def __setattr__(self, key, value):
    self[key] = value
```

错写成

```
def __setattr(self, key, value):
    self[key] = value
```

导致设置属性的逻辑根本无法生效

这也正是通过单元测试发现的bug，正好展示了unittest 的用处
