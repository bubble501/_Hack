>转载[《Python 网页爬虫 & 文本处理 & 科学计算 & 机器学习 & 数据挖掘兵器库》](http://blog.csdn.net/mack415858775/article/details/40182187)

---

有很多的Python工具包，特别是在文本处理、科学计算、机器学习和数据挖掘领域

这里尝试整理一套Python网页爬虫、文本处理、科学计算、机器学习和数据挖掘的兵器谱

## Python网页爬虫工具集

一个真实的项目，一定是从获取数据开始的。无论文本处理、机器学习和数据挖掘，都需要数据，除了通过一些渠道购买或者下载的专业数据外，常常需要大家自己动手爬数据，这个时候，爬虫就显得格外重要了，幸好，Python提供了一批很不错的网页爬虫工具框架，既能爬取数据，也能获取和清洗数据，我们也就从这里开始了！

### [Scrapy](http://scrapy.org/)

>Scrapy，a fast high-level screen scraping and web crawling framework for Python

[课程图谱](http://coursegraph.com/)中的很多课程都是依靠Scrapy抓去的，这方面的介绍文章很多，推荐大牛pluskid早年的一篇文章[《Scrapy轻松定制网络爬虫》](http://blog.pluskid.org/?p=366)，历久弥新

* 官方主页：http://scrapy.org/
* Github代码页：https://github.com/scrapy/scrapy

### [Beautiful Soup](http://www.crummy.com/software/BeautifulSoup/)

>You didn't write that awful page. You're just trying to get some data out of it. Beautiful Soup if here to help. Since 2004, it's been saving programmers hours of days of work on quick-turnaround screen scraping projects

《集体编程智慧》这本书中有讲到Beautiful Soup。客观的说，Beautiful Soup 不完全是一套爬虫工具，需要配合urllib使用，而是一套HTML/XML数据分析，清洗和获取工具

* 官方主页：http://www.crummy.com/software/BeautifulSoup/

### [Python-Goose](https://github.com/grangier/python-goose)

>Html Content/Article Extractor, web scrapping lib in Python

Goose最早是用Java写的，后来用Sacla重写，是一个Scala项目。Python-Goose用Python重写，依赖了Beautiful Soup。给定一个文章的URL，获取文章的标题和内容很方便

* Github主页：https://github.com/grangier/python-goose

## Python文本处理工作集

从网页上获取文本数据之后，根据任务的不同，就需要进行基本的文本处理了，譬如对于英文来说，需要基本的tokenize，对于中文，则需要常见的中文分词，进一步的话，无论英文中文，还可以词性标注、句法分析、关键词提取、文本分类、情感分析等。这方面，特别是面向英文领域，有很多的工具包

### [NLTK](http://www.nltk.org/)

>NLTK(Natural Language Toolkit) is a leading platform for building Python programs to work with human language data. It provides easy-to-use interfaces to over 50 corpora and lexical resources such as WordNet, along with a suite of text processing libraries for classification, tokenization, stemming, tagging, parsing, and semantic reasoning, and an active discussion forum.

推荐两本书给刚接触NLTK或者需要详细了解NLTK的同学：

* 官方的《Natural Language Processing with Python》
	* 以介绍NLTK里的功能用法为主，同时附带一些Python知识
	* 另有中文版[《用Python进行自然语言处理》中文翻译-NLTK配套书](http://www.52nlp.cn/%E6%8E%A8%E8%8D%90%EF%BC%8D%E7%94%A8python%E8%BF%9B%E8%A1%8C%E8%87%AA%E7%84%B6%E8%AF%AD%E8%A8%80%E5%A4%84%E7%90%86%EF%BC%8D%E4%B8%AD%E6%96%87%E7%BF%BB%E8%AF%91-nltk%E9%85%8D%E5%A5%97%E4%B9%A6)
* 另一本是《Python Text Processing with NLTK 2.0 CookBook》
	* 这本书要深入一些，会涉及到NLTK的代码结构
	* 同时会介绍如何定制自己的语料和模型等

* 官方主页：http://www.nltk.org/
* Github代码页：https://github.com/nltk/nltk

## [Pattern](http://www.clips.ua.ac.be/pattern)

> Pattern is a web mining module for the Python programming language. It has tools for data mining(Google, Twitter and Wikipedia API, a web crawler, a HTML DOM parser), natural language processing(part-of-speech taggers, n-gram search, sentiment analysis, WordNet), machine learning(vector space model, clustering, SVM), network analysis and canvas visualization

Pattern不仅仅是一套文本处理工具，更是一套web数据挖掘工具，囊括了数据抓取模块（包括Google、Twitter、维基百科的API，以及爬虫和HTML分析器），文本处理模块（词性标注、情感分析等），机器学习模块（VSM、聚类、SVM）以及可视化模块等，可以说，Pattern的这一整套逻辑也是这篇文章的组织逻辑，不过这里我们暂且把Pattern放到文本处理部分。我个人主要使用的是它的英文处理模块[Pattern.en](http://www.clips.ua.ac.be/pages/pattern-en)，有很多很不错的文本处理功能，包括基础的tokenize、词性标注、句子切分、词法检查、拼写纠错、情感分析、句法分析等，相当不错

* 官方主页：http://www.clips.ua.ac.be/pattern

### [TextBlob](http://textblob.readthedocs.org/en/dev/)

>TextBlob(Simplified Text Processing) is a Python(2 and 3) library for processing textual data. It provides a simple API for diving into common language processing(NLP) tasks such as part-of-speech tagging, noun phrase extraction, sentiment analysis, classification, tranlation, and more

TextBlob是一个很有意思的Python文本处理工具包，它其实是基于上面两个Python工具包NLKT和Pattern做了封装（TextBlob stands on the giant shoulders of NLTK and pattern, and plays nicely with both），同时提供了很多文本处理功能的接口，包括词性标注、名词短语提取、情感分析、文本分类、拼写检查等，甚至包括翻译和语言检测，不过这个是基于Google的API的，有调用次数限制。TextBlob相对比较年轻，有兴趣的同学可以关注

* 官方主页：http://textblob.readthedocs.org/en/dev/
* Github代码页：https://github.com/sloria/textblob

### [MBSP](http://www.clips.ua.ac.be/pages/MBSP) for Python

>MBSP is a text analysis system based on the TiMBL and MBT memory based learning applications developed at CLiPS and ILK. It provides tools for Tokenization and Sentence Splitting, Part of Speech Tagging, Chunking, Lemmatization, Relation Finding and Prepositional Phrase Attachment.

MBSP与Pattern同源，同出于比利时安特卫普大学CLiPS实验室，提供了Word Tokenization、句子切分、词性标注、Chunking、Lemmatization、句法分析等基本的文本处理功能

* 官方主页：http://www.clips.ua.ac.be/pages/MBSP

### [Gensim](http://radimrehurek.com/gensim/index.html): Topic modeling for humans

Gensim是一个相当专业的主题模型Python工具包，无论是代码还是文档，我们曾经用[《如何计算两个文档的相似性》](http://www.52nlp.cn/%E5%A6%82%E4%BD%95%E8%AE%A1%E7%AE%97%E4%B8%A4%E4%B8%AA%E6%96%87%E6%A1%A3%E7%9A%84%E7%9B%B8%E4%BC%BC%E5%BA%A6%E4%B8%80)介绍过Gensim的安装和使用过程，这里就不多说了

* 官方主页：http://radimrehurek.com/gensim/index.html
* Github代码页：https://github.com/piskvorky/gensim

### [langid.py](https://github.com/saffsd/langid.py): Stand-alone language identification system

语言检测是一个很有意思的话题，不过相对比较成熟，这方面的解决方案很多，也有很多不错的开源工具包，不过对于Pythhon来说，我使用过langid这个工具包，也非常愿意推荐它。langid目前支持97种语言的检测，提供了很多易用的功能，包括可以启动一个建议的Server，通过Json调用其API，可定制训练自己的语言检测模型等，可以说是“麻雀虽小五脏俱全”

* Github主页：https://github.com/saffsd/langid.py

### [Jieba](https://github.com/fxsjy/jieba): 结巴中文分词

国内的一个Python文本处理工具包，其功能包括三种分词模式（精确模式、全模式、搜索引擎模式），支持繁体分词、支持自定义词典等，是目前一个非常不错的Python中文分词解决方案

* Github主页：https://github.com/fxsjy/jieba

### [xTAS](https://github.com/NLeSC/xtas)

>xTAS, the eXtensible Text Analysis Suite, a distributed text analysis package based on Celery and Elasticsearch

* Github代码页：https://github.com/NLeSC/xtas

## Python科学计算工具包

说到科学计算，大家首先想到的是Matlab，集数值计算、可视化工具及交互于一身，不过可惜是一个商业产品。开源方面除了[GNU Octave](http://www.gnu.org/software/octave/)在尝试做一个类似Matlab的工具包外，Python的这几个工具包集合到一起也可以替代Matlab的相应功能：Numpy+SciPy+Matplotlib+iPython。同时，这几个工具包，特别是Numpy和SciPy，也是很多Python文本处理、机器学习和数据挖掘工具包的基础，非常重要

最后推荐一个系列[《用Python做科学计算》](http://sebug.net/paper/books/scipydoc/index.html)，将会涉及到NumPy、SciPy、Matplotlib，可以做参考

### [NumPy](http://www.numpy.org/)

NumPy is the fundamental package for scientific computing with Python. It conatins among other things:

* a powerful N-dimensional array object
* sophisticated(broadcasting) functions
* tools for intergrating C/C++ and Fortran code
* useful linear algebra, Fourier transform, and randonm number capabilities

Besides its obvious scientific uses, NumPy can also be used as an efficient multi-dimensional Container of generic data. Arbitrary data-types can be defined. This allows NumPy to seamlessly and speedily intergrate with a wide variety of databases

NumPy几乎是一个无法回避的科学计算工具包，最常用的也许是它的N维数组对象，其他还包括一些成熟的函数库，用于整合C/C++和Fortran代码的工具包，线性代数、傅里叶变换和随机数生成函数等。NumPy提供了两种基本的对象：ndarray（N-dimensional array object）和ufunc（universal function obejct）。ndarray是存储单一数据类型的多维数组，而ufunc则是能够对数组进行处理的函数

* 官方主页：http://www.numpy.org/

### [Pandas](http://pandas.pydata.org/): Python Data Analysis Library

>Pandas is a software library written for the Python programming language for data manipulation and analysis. In particular, it offers data structures and operations for manipulating numerical tables and time series.

第一次接触Pandas是由于Udacity上的一门数据分析课程[“Introduction to Data Science”](http://coursegraph.com/introduction-to-data-science-udacity-ud359-%E5%85%B6%E4%BB%96%E5%A4%A7%E5%AD%A6%E6%88%96%E6%9C%BA%E6%9E%84) 的Project需要用Pandas库，所以学习了一下Pandas。Pandas也是基于NumPy和Matplotlib开发的，主要用于数据分析和数据可视化，它的数据结构DataFrame和R语言里的data.frame很像，特别是对于时间序列数据有自己的一套分析机制，非常不错。这里推荐一本书[《Python for Data Analysis》](http://bin.sc/Readings/Programming/Python/Python%20for%20Data%20Analysis/Python_for_Data_Analysis.pdf)，作者是Pandas的主力开发，依次介绍了iPython, NumPy, Pandas里的相关功能，数据可视化，数据清洗和加工，时间数据处理等，案例包括金融股票数据挖掘等，相当不错。

* 官方主页：http://pandas.pydata.org/

---

>以上工具包基本上都是自己用过的，以下来源于其他同学的线索，特别是[《Python机器学习库》](http://qxde01.blog.163.com/blog/static/67335744201368101922991/)，[《23个python的机器学习包》](http://52opencourse.com/1125/23%E4%B8%AApython%E7%9A%84%E6%9C%BA%E5%99%A8%E5%AD%A6%E4%B9%A0%E5%8C%85)，做了一点增删修改，欢迎大家补充

---

### [mlpy](http://mlpy.sourceforge.net/) – Machine Learning Python

>mlpy is a Python module for Machine Learning built on top of NumPy/SciPy and the GNU Scientific Libraries.
mlpy provides a wide range of state-of-the-art machine learning methods for supervised and unsupervised problems and it is aimed at finding a reasonable compromise among modularity, maintainability, reproducibility, usability and efficiency. mlpy is multiplatform, it works with Python 2 and 3 and it is Open Source, distributed under the GNU General Public License version 3.

* 官方主页：http://mlpy.sourceforge.net/

### [MDP](http://mdp-toolkit.sourceforge.net/)：The Modular toolkit for Data Processing

>Modular toolkit for Data Processing (MDP) is a Python data processing framework.
From the user’s perspective, MDP is a collection of supervised and unsupervised learning algorithms and other data processing units that can be combined into data processing sequences and more complex feed-forward network architectures.

>From the scientific developer’s perspective, MDP is a modular framework, which can easily be expanded. The implementation of new algorithms is easy and intuitive. The new implemented units are then automatically integrated with the rest of the library.

>The base of available algorithms is steadily increasing and includes signal processing methods (Principal Component Analysis, Independent Component Analysis, Slow Feature Analysis), manifold learning methods ([Hessian] Locally Linear Embedding), several classifiers, probabilistic methods (Factor Analysis, RBM), data pre-processing methods, and many others.

“MDP用于数据处理的模块化工具包，一个Python数据处理框架。从用户的观点，MDP是能够被整合到数据处理序列和更复杂的前馈网络结构的一批监督学习和非监督学习算法和其他数据处理单元。计算依照速度和内存需求而高效的执行。从科学开发者的观点，MDP是一个模块框架，它能够被容易地扩展。新算法的实现是容易且直观的。新实现的单元然后被自动地与程序库的其余部件进行整合。MDP在神经科学的理论研究背景下被编写，但是它已经被设计为在使用可训练数据处理算法的任何情况中都是有用的。其站在用户一边的简单性，各种不同的随时可用的算法，及应用单元的可重用性，使得它也是一个有用的教学工具。”

* 官方主页：http://mdp-toolkit.sourceforge.net/

### [PyBrain](http://www.pybrain.org/)

>PyBrain is a modular Machine Learning Library for Python. Its goal is to offer flexible, easy-to-use yet still powerful algorithms for Machine Learning Tasks and a variety of predefined environments to test and compare your algorithms.
PyBrain is short for Python-Based Reinforcement Learning, Artificial Intelligence and Neural Network Library. In fact, we came up with the name first and later reverse-engineered this quite descriptive “Backronym”.

>“PyBrain(Python-Based Reinforcement Learning, Artificial Intelligence and Neural Network)是Python的一个机器学习模块，它的目标是为机器学习任务提供灵活、易应、强大的机器学习算法。（这名字很霸气）

PyBrain正如其名，包括神经网络、强化学习(及二者结合)、无监督学习、进化算法。因为目前的许多问题需要处理连续态和行为空间，必须使用函数逼近(如神经网络)以应对高维数据。PyBrain以神经网络为核心，所有的训练方法都以神经网络为一个实例。”

* 官方主页：http://www.pybrain.org/

### [PyML](http://pyml.sourceforge.net/) – machine learning in Python

>PyML is an interactive object oriented framework for machine learning written in Python. PyML focuses on SVMs and other kernel methods. It is supported on Linux and Mac OS X.

“PyML是一个Python机器学习工具包，为各分类和回归方法提供灵活的架构。它主要提供特征选择、模型选择、组合分类器、分类评估等功能。”

* 项目主页：http://pyml.sourceforge.net/

### [Milk](https://pypi.python.org/pypi/milk/)：Machine learning toolkit in Python.

>Its focus is on supervised classification with several classifiers available: SVMs (based on libsvm), k-NN, random forests, decision trees. It also performs feature selection. These classifiers can be combined in many ways to form different classification systems.

>“Milk是Python的一个机器学习工具箱，其重点是提供监督分类法与几种有效的分类分析：SVMs(基于libsvm)，K-NN，随机森林经济和决策树。它还可以进行特征选择。这些分类可以在许多方面相结合，形成不同的分类系统。对于无监督学习，它提供K-means和affinity propagation聚类算法。”

* 官方主页：http://luispedro.org/software/milk

### [PyMVPA](http://www.pymvpa.org/): MultiVariate Pattern Analysis (MVPA) in Python

>PyMVPA is a Python package intended to ease statistical learning analyses of large datasets. It offers an extensible framework with a high-level interface to a broad range of algorithms for classification, regression, feature selection, data import and export. It is designed to integrate well with related software packages, such as scikit-learn, and MDP. While it is not limited to the neuroimaging domain, it is eminently suited for such datasets. PyMVPA is free software and requires nothing but free-software to run.

“PyMVPA(Multivariate Pattern Analysis in Python)是为大数据集提供统计学习分析的Python工具包，它提供了一个灵活可扩展的框架。它提供的功能有分类、回归、特征选择、数据导入导出、可视化等”

* 官方主页：http://www.pymvpa.org/

### [Pyrallel](https://github.com/pydata/pyrallel) – Parallel Data Analytics in Python

>Experimental project to investigate distributed computation patterns for machine learning and other semi-interactive data analytics tasks.

“Pyrallel(Parallel Data Analytics in Python)基于分布式计算模式的机器学习和半交互式的试验项目，可在小型集群上运行”

* Github代码页：http://github.com/pydata/pyrallel

### [Monte](http://montepython.sourceforge.net/) – gradient based learning in Python

>Monte (python) is a Python framework for building gradient based learning machines, like neural networks, conditional random fields, logistic regression, etc. Monte contains modules (that hold parameters, a cost-function and a gradient-function) and trainers (that can adapt a module’s parameters by minimizing its cost-function on training data).

>Modules are usually composed of other modules, which can in turn contain other modules, etc. Gradients of decomposable systems like these can be computed with back-propagation.

“Monte (machine learning in pure Python)是一个纯Python机器学习库。它可以迅速构建神经网络、条件随机场、逻辑回归等模型，使用inline-C优化，极易使用和扩展。”

* 官方主页：http://montepython.sourceforge.net

### [Theano](http://deeplearning.net/software/theano/)

>Theano is a Python library that allows you to define, optimize, and evaluate mathematical expressions involving multi-dimensional arrays efficiently. Theano features:

* tight integration with NumPy – Use numpy.ndarray in Theano-compiled functions.
* transparent use of a GPU – Perform data-intensive calculations up to 140x faster than with CPU.(float32 only)
* efficient symbolic differentiation – Theano does your derivatives for function with one or many inputs.
* speed and stability optimizations – Get the right answer for log(1+x) even when x is really tiny.
* dynamic C code generation – Evaluate expressions faster.
* extensive unit-testing and self-verification – Detect and diagnose many types of mistake. Theano has been powering large-scale computationally intensive scientific investigations since 2007. But it is also approachable enough to be used in the classroom (IFT6266 at the University of Montreal).

“Theano 是一个 Python 库，用来定义、优化和模拟数学表达式计算，用于高效的解决多维数组的计算问题。Theano的特点：紧密集成Numpy；高效的数据密集型GPU计算；高效的符号微分运算；高速和稳定的优化；动态生成c代码；广泛的单元测试和自我验证。自2007年以来，Theano已被广泛应用于科学运算。theano使得构建深度学习模型更加容易，可以快速实现多种模型。PS：Theano，一位希腊美女，Croton最有权势的Milo的女儿，后来成为了毕达哥拉斯的老婆。”

### [Pylearn2](http://deeplearning.net/software/pylearn2/)

>Pylearn2 is a machine learning library. Most of its functionality is built on top of Theano. This means you can write Pylearn2 plugins (new models, algorithms, etc) using mathematical expressions, and theano will optimize and stabilize those expressions for you, and compile them to a backend of your choice (CPU or GPU).

“Pylearn2建立在theano上，部分依赖scikit-learn上，目前Pylearn2正处于开发中，将可以处理向量、图像、视频等数据，提供MLP、RBM、SDA等深度学习模型。”

* 官方主页：http://deeplearning.net/software/pylearn2/

## 相关文章:

* [Google’s Python Class](http://www.52nlp.cn/googles-python-class)
* [Google’s Python Class – SOS](http://www.52nlp.cn/googles-python-class-sos)
* [如何计算两个文档的相似度（二）](http://www.52nlp.cn/%e5%a6%82%e4%bd%95%e8%ae%a1%e7%ae%97%e4%b8%a4%e4%b8%aa%e6%96%87%e6%a1%a3%e7%9a%84%e7%9b%b8%e4%bc%bc%e5%ba%a6%e4%ba%8c)
* [Google’s Python Class SOS 续 –下载](http://www.52nlp.cn/googles-python-class-sos-%e7%bb%ad-%e4%b8%8b%e8%bd%bd)
* [几种不同程序语言的HMM版本](http://www.52nlp.cn/several-different-programming-language-version-of-hmm)
* [如何计算两个文档的相似度（三）](http://www.52nlp.cn/%e5%a6%82%e4%bd%95%e8%ae%a1%e7%ae%97%e4%b8%a4%e4%b8%aa%e6%96%87%e6%a1%a3%e7%9a%84%e7%9b%b8%e4%bc%bc%e5%ba%a6%e4%b8%89)
* [推荐《用Python进行自然语言处理》中文翻译-NLTK配套书](http://www.52nlp.cn/%e6%8e%a8%e8%8d%90%ef%bc%8d%e7%94%a8python%e8%bf%9b%e8%a1%8c%e8%87%aa%e7%84%b6%e8%af%ad%e8%a8%80%e5%a4%84%e7%90%86%ef%bc%8d%e4%b8%ad%e6%96%87%e7%bf%bb%e8%af%91-nltk%e9%85%8d%e5%a5%97%e4%b9%a6)
* [条件随机场文献阅读指南](http://www.52nlp.cn/%e6%9d%a1%e4%bb%b6%e9%9a%8f%e6%9c%ba%e5%9c%ba%e6%96%87%e7%8c%ae%e9%98%85%e8%af%bb%e6%8c%87%e5%8d%97)
* [MapReduce与自然语言处理](http://www.52nlp.cn/mapreduce%e4%b8%8e%e8%87%aa%e7%84%b6%e8%af%ad%e8%a8%80%e5%a4%84%e7%90%86)
* [分享：ConceptBro语义网浏览器](http://www.52nlp.cn/%e5%88%86%e4%ba%ab%ef%bc%9aconceptbro%e8%af%ad%e4%b9%89%e7%bd%91%e6%b5%8f%e8%a7%88%e5%99%a8)
