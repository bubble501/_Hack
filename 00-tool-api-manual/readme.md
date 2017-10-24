之前我在[www.xumenger.com](www.xumenger.com)上整理了很多文章，但仔细看除了一些关于结构体对齐机制、C++对象内存模型、WinDbg调试、简单网络编程的一些东西之外，其他的很多文章真的就是各种API用法的展示而已，现在想想没有任何技术含量

最近还想整理2篇关于Linux下一些工具使用方法的说明文章，现在想想确实没有啥必要，这些术层面的东西不要过分浪费自己的时间，还不如花这些时间实实在在地去写代码、做开发、学习其他知识

就像当初看《射雕英雄传》，当时江南七怪出来的时候，又是大侠，又是高手的，以为很厉害，结果后面在蛤蟆功、降龙十八掌等面前简直就是小儿科，一个个大侠也都被虐的体无完肤

自己现在做的东西就停留在江南七怪的功夫水平上，根本不是什么高手，就是个搬砖工而已

不过像很多API的用法、命令行的用法也还是有用处的。所以我就专门将一些常用的API、命令整理在这里，随时添加其他常用的到的，并且作为自己平时快速查看的一个小纸条

以API为例，还是建议去参考官方手册，认真看其接口规范、参数要怎么传、返回值有什么用？

会有以下方面的总结：

* Linux命令的用法
* delphi、python、c、c++、matlab、lisp、R等语言的用法
* 常用的正则表达式
* 常用的SQL
* 等等

这里的各个小文章只是一些私人的经验性的小总结！并不是专门整理，而是在自己逐渐使用的过程中进行迭代式添加，同时注意各个Manual的文档格式要简洁清晰！

## 官方文档的重要性

不要参考所谓的经验性博客，而应该去直接参考官方的接口文档、官方标准文档。比如对于API，仔细研究每个参数的作用、考虑到每一种可能的返回值，只有官方接口文档的说明最可信

这一点是自己必须要注意，之前的开发中这点做得很不规范，总是在网络上随便找了一篇博客做参考，也不知道其正确性如何！

* [MSDN: Windows API查询](https://msdn.microsoft.com/zh-cn/)
* [cppreference: C++语法查询](http://en.cppreference.com/w/)
* [Python2.7.8官方文档](http://python.usyiyi.cn/translate/python_278/index.html)
* [Racket参考手册](http://docs.racket-lang.org/reference/)
* [Flask中文文档](http://docs.jinkan.org/docs/flask/)
* [Scrapy接口文档](http://python.usyiyi.cn/translate/scrapy_14/index.html)
* Linux下有问题直接找`man`
* [所有的RFC](https://tools.ietf.org/rfc/index)
* [更多文档](http://python.usyiyi.cn/)
