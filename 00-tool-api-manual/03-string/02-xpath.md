需要先`pip install lxml`

表达式   |描述
---------|-----
nodename |选取此节点的所有子节点，tag或\*选择任意的tag
/        |从根节点选取，选择直接子节点，不包含更小的后代（例如孙、重孙）
//       |从匹配选择的当前节点选择文档中的节点，而不考虑它们的位置，包含所有后代
.        |选取当前节点
..       |选取当前节点的父节点
@        |选取属性

## 简单用法示例

```
>>> import codecs
>>> from lxml import etree
>>> html = '<a rel="nofollow" class="external text" href="www.xumenger.com">xumenger<wbr />.ac</a>'
>>> tree = etree.HTML(html)
>>> result = tree.xpath('//a')
>>> print result
[<Element a at 0x1023eac68>]
>>> print result[0].text
xumenger
>>> result = tree.xpath('//*[@class="external text"]')
>>> print result[0].text
xumenger
>>> result = tree.xpath('//*[contains(@class, "external")]')
>>> print result[0].text
xumenger
>>> print result[0].tag
a
```

## 运算符

```
#选择p或者span或者h1标签的元素
>>> result = tree.xpath('//td[@class="editor bbsDetailContainer"]//*[self::p of self::span or self::h1]')

#选择class为editor或tag的元素
result = tree.xpath('//td[@class="editor" or @class="tag"]')
```

