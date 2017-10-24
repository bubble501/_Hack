## 正则表达式匹配规则

```
>>> import re
>>> html = '<link rel="icon" sizes="any" mask href="//www.baidu.com/img/baidu.svg">'
>>> pattern1 = '<link rel="icon" sizes="any" mask href=".+?">'
>>> url1 = re.compile(pattern1).findall(html)
>>> print(url1)
['<link rel="icon" sizes="any" mask href="//www.baidu.com/img/baidu.svg">']
>>> pattern2 = '<link rel="icon" sizes="any" mask href="(.+?)">'
>>> url2 = re.compile(pattern2).findall(html)
>>> print(url2)
['//www.baidu.com/img/baidu.svg']
```

上面展示了正则表达式的用法：

第一种没有使用`()`，所以最后解析出来，用findall()获取的内容是包含了完整的内容

而第二种在正则表达式中使用了`()`，最后解析出来的是符合正则表达式，但只包含在`()`中的内容

所以使用Python的正则表达式解析的时候，`()`的使用需要特别注意！其实之前做爬虫开发的时候对这个细节点一直没有弄清楚，困惑我许久的一个点终于在今天弄明白了，就是一个小小的`()`细节点！

>下面整理一些开发中常用的正则表达式

## 解析HTML中的URL

```
import re

# 先准备好待解析的HTML页面

# 使用正则表达式解析页面
pattern = '(https?://[^\s)";]+\.(\w|/)*)'
urls = re.compile(pattern).findall(html)

# 去除重复元素
urls = list(set(urls))

# 循环输出URL
for url in urls:
    print(url[0])
```

