## 说明

本次练习参考自[《HTML5和CSS3扁平化风格博客》](http://www.imooc.com/learn/445?from=jobboleblog)

本课程通过CSS3扁平化风格博客的实例详细讲解，会更容易理解常规网页的组成部分，灵活地使用不同部分Web组件，理解它的构成思想，以及网页灵活的布局和设计

由宏观到微观去掌握整体的布局

## 练习的目的

理解网页布局的结构，无论是什么网页，它最基本的结构都是一样的，有一个最基本的共同点

掌握网页布局的基本步骤，如何在网页的布局的基础上实现它

掌握博客类网页的布局思想

## 模块分析

每开发一个项目之前，我们首先要对一个项目做一个大致的规划，能有一个宏观的了解：

* 要实现什么样的功能
* 要满足什么样的需求
* ……

然后再去把这些东西具象化

有了宏观的了解之后，再去将宏观的东西进行细分，细分成更小的模块，然后就可以逐一击破

如果我们不做模块分析，一上手想到哪就写到哪的话，这样就很容易一叶障目，不能看到全貌，可能就会导致做着做着，忽然就发现失控了，因为一个地方和另一个地方发生了矛盾，这时候可能是修改的成本很高，还不如从头再写！这是在最开始做编程的时候很容易犯的错误：容易着眼于细节，一开始就从细节开始做！

其实首先不应该从细节开始做，首先要分析不同的模块！

首先规划一下这个博客页面的整体结构：

* 页头部分
* 主体部分
* 页脚部分

每做一张网页，首先去考虑它的结构，其次是去考虑它的样式。我们很少听人说HTML 重要，其实是因为HTML 实在是太重要了，以至于不需要再说，就比如很少听到人说空气重要、阳光重要！

## 重置样式

浏览器对于一些标签会有自己默认的样式，比如h1 标签默认的字体大小、加粗、边距……

重置样式的目的

* 使样式在不同浏览器下保持一致
* 有一些浏览器的默认样式确实没有用

但不要完全重置，如果完全重置，那么h1 标签的内容看起来和p 标签的内容一样了，所以还是保留必要的样式，将一些无用的样式reset 即可！这时候就可以使用normalize.css 来实现这样的效果

## 页头、Banner、正文、页脚的宏观布局

在宏观布局的时候，我们不在乎它的细节，还是那句话，顺序要从大到小，先着眼最大的部分，再逐步实现细节

* 首先在index.html 中分为三部分：页头、主体、页脚
* 除了normalize.css，使用main.css 作为整个项目的样式文件

在完成页面整个大框架的建立之后，就可以逐个的实现一个个具体的子模块

## 总结

这次一步一步跟着教程，最终实现出来的网页的效果不是很好，而且代码也是组织的比较乱的

关于HTML5、CSS3 的知识这次练习仅仅只是皮毛

后续的练习：

* 有必要针对这个小练习持续优化
* 有必要做其他更多的练习

后面需要达到的程度

* 熟练掌握使用HTML 进行网页布局
* 熟练掌握CSS 的重要样式属性
* 清楚CSS 为各个元素设置属性之后的优先级效果
* 现在的CSS 样式明显混在一起，怎么合理规划，使得作用到HTML 元素上的代码逻辑更清晰？
* 怎么用更少的HTML 和CSS 做出相同的效果，以在网络传输时更节省带宽？

## 网页开发过程中的一些建议

**搜索引擎优化**

* 一个页面的h2、h3 等可以有多个
* 但建议一个页面只有一个h1 标签
* 这样对于搜索引擎的优化是很好的

**边开发边测试**

* 边写网页的代码，边使用Chrome、FireFox 等浏览器实时查看显示的效果
* 有不合理的立即修改
* 这个和平时做Delphi、C、C++、Python 开发时的边开发边测试是一样的！
* 直接在浏览器的开发者工具下进行修改，更加可以及时的看到修改后的页面效果！

**分CSS 文件注意**

* 有的人可能会把CSS 分成很多个文件，比如header 部分的分一个、主体部分分一个、footer 部分分一个
* 这种方法当然是在功能上没有任何问题的
* 但这样做的结果是平白无故地增加浏览器的请求数，每多分出来一个CSS 文件，对应浏览器也就要多请求一次
* 浏览器的请求数越多，当然也就越慢
* 机器的处理速度所能造成的延迟，其实和网速相比是微不足道的
* 所以一张网页的加载的快慢很大程度上取决于页面中请求数目的多少
* 所以在真实的项目中还是建议将CSS 压缩到尽可能少的文件中，一两个应该也就够了，太多就没有任何意义
* 如果划分为多个文件，其实浏览器最终在解析时，还是将所有的文件先首尾连接到一起再去解析的
