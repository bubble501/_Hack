[Python量化交易平台开发教程系列7-顶层GUI界面开发（1）](http://www.vnpy.org/basic-tutorial-7.html)

# 前言

终于有时间来写第一篇顶层GUI界面开发相关的教程了，之前实在是事情太多，跟各位读者抱个歉

整合底层接口的各项功能到中层引擎后，当我们开发顶层应用时（GUI或者策略算法），只需要知道中层引擎对外提供的主动API函数以及事件引擎中相关的事件类型和数据形式即可

在GUI和策略算法这两个主要类型的顶层应用中，作者选择先介绍GUI开发的原因是：目前国内支持用户定制化开发GUI界面的量化平台少之又少，而包含一个比较全面的GUI开发教程的则据我所知还没有。随着国内越来越多的衍生品的推出（期权、分级基金、未来的反向基金），很多新型的交易策略从全自动转向了半自动，经常需要交易员的手动干预（启动暂停策略、盘中微调参数等），以及投资组合层面的风险管理（期权希腊值、分级基金行业暴露等），这种情况下传统上仅支持策略算法开发的量化交易平台变得越发难以满足交易员的需求（包括作者本人），所以估计这方面的文章更能填补当前市场需求的空缺

# PyQt

目前Python上主要的GUI开发工具包括：tkinter、PyQt、PyGTK和wxPython，作者选择PyQt的主要原因是：

* Anaconda中已经包含（早期版本中包含的是LGPL协议的PySide，稳定性不如PyQt）
* 另一个内置的GUI库tkinter功能太弱

网上对这四款GUI开发工具比较分析的文章很多，有兴趣的读者可以自己搜搜看

接下来的几篇GUI开发教程会假设读者已经对PyQt开发有了一定的了解，主要针对和量化交易平台相关的部分，需要补充基础知识大读者建议参考以下资源：

* Rapid GUI Programming with Python and Qt，一本由Riverbank（PyQt的开发公司）员工推出的PyQt开发教程，非常细致全面，但是部分内容跳跃性太大，需要从头到尾多看几遍
* 也可以在遇到特定问题的时候搜百度或Stack Overflow，通常都能找到答案

PyQt版本在[Riverbank官网](http://www.riverbankcomputing.com/software/pyqt/download)下载：

>PyQt4-4.11.4-gpl-Py2.7-Qt4.8.7-x32.exe Windows 32 bit installer

请特别注意这个版本的问题，从最近几个月的经验看，很多PyQt相关模块运行时报错就是因为下错了版本（PyQt5、64位等等都不对）

# GUI组件

从功能上看，所有交易平台的GUI组件都可以分为两类，数据监控（被动）和功能调用（主动），当然也有同时混合两类功能的组件

### 数据监控

![image](./image/07-01.jpg)

行情监控组件，用于监控实时行情数据，每当API端有新的行情数据推送时立即进行更新

### 功能调用

![image](./image/07-02.jpg)

账号登录组件，用于调用中层引擎的登录功能，传入用户名、密码、服务器地址等参数

### 混合（交易下单）

![image](./image/07-03.jpg)

交易下单组件，左侧部分用于填入下单参数后调用中层引擎的下单功能发单，右侧部分用于监控用户输入的合约代码的实时行情（有行情推送时立即更新）

# 数据监控组件

数据监控组件主要用于对交易平台中的各项数据实现实时更新或者手动更新的显示监控，最常用的包括行情、报单、成交、持仓和日志等。监控组件中最常见的类型是表格，对应PyQt中的QTableWidget组件，表格中的单元格则使用QTableWidgetItem组件。对于某些特殊的数据监控也可以使用其他类型的组件，如上面交易下单组件中右侧的部分，主要是使用标签组件QLabel构成的

针对不同的监控内容需要实现不同的数据更新方法，例如日志、成交类数据应该使用插入更新（即每条新的数据都应该插入新的一行），行情数据应该使用固定位置更新（即在表格中固定的单元格位置更新数据），以及主要针对持仓和报单数据的混合更新（即已经存在的数据直接在对应的位置更新，否则插入新的一行）

下面以最基本的日志监控为例介绍监控组件的实现原理，其他更为复杂的监控组件建议用户直接阅读vn.demo中的demoMain.py的源代码，大部分代码作者都做了详尽的注释

# 日志监控

日志监控组件主要用于输出程序运行过程中有关当前程序运行状态的信息

![image](./image/07-04.jpg)

整个实现代码如下：

```
class LogMonitor(QtGui.QTableWidget):
    """用于显示日志"""
    signal = QtCore.pyqtSignal(type(Event()))

    def __init__(self, eventEngine, parent=None):
        super(LogMonitor, self).__init__(parent)
        self.__eventEngine = eventEngine

        self.initUi()
        self.registerEvent()

    def initUi(self):
        """初始化界面"""
        self.setWindowsTitle(u'日志')
        
        self.setColumnCount(2)
        self.setHorizontalHeaderLabels([u'时间', u'日志'])

        self.verticalHeader.setVisible(False)                   # 关闭左边的垂直表头
        self.setEditTriggers(QtGui.QTableWidget.NoEditTriggers) # 设为不可编辑状态
        
        # 自动调整列宽
        self.horizontalHeader().setResizeMode(0, QtGui.QHeaderView.ResizeToContents)
        self.horizontalHeader().setResizeMode(0, QtGui.QHeaderView.Stretch)

    def registerEvent(self):
        """注册事件监听"""
        # Qt图形组件的GUI更新必须使用Signal/Slot机制，否则有可能导致程序崩溃
        # 因此这里先将图形更新函数作为Slot，和信号连接起来
        # 然后将信号的触发函数注册到事件驱动引擎中
        self.signal.connect(self.updateLog)
        self.__eventEngine.register(EVENT_LOG, self.signal.emit)

    def updateLog(self, event):
        """更新日志"""
        # 获取当前时间和日志内容
        t = time.strftime('%H:%M:%S', time.localtime(time.time()))
        log = event.dict_['log']

        # 在表格最上方插入一行
        self.insertRow(0)

        # 创建单元格
        cellTime = QtGui.QTableWidgetItem(t)
        cellLog = QtGui.QTableWidgetItem(log)

        # 将单元格插入表格
        self.setItme(0, 0, cellTime)
        self.setItem(0, 1, cellLog)
```

接下来逐段讲解

**对象初始化（init）**

```
    def __init__(self, eventEngine, parent=None):
        """Constructor"""
        super(LogMonitor, self).__init__(parent)
        self.__eventEngine = eventEngine

        self.initUi()
        self.registerEvent()
```

* 创建对象时，我们需要传入程序中的事件驱动引擎对象eventEngine，以及该图形组件所依附的母组件对象parent（一般可以留空）
* 把eventEngine对象的引用保存在_eventEngine上，我们调用initUi方法初始化图形组件的界面，以及registerEvent方法来向事件引擎中注册该图形组件的事件监听函数

**初始化界面（initUi）**

```
    def initUi(self):
        """初始化界面"""
        self.setWindowTitle(u'日志')

        self.setColumnCount(2)                     
        self.setHorizontalHeaderLabels([u'时间', u'日志'])

        self.verticalHeader().setVisible(False)                 # 关闭左边的垂直表头
        self.setEditTriggers(QtGui.QTableWidget.NoEditTriggers) # 设为不可编辑状态

        # 自动调整列宽
        self.horizontalHeader().setResizeMode(0, QtGui.QHeaderView.ResizeToContents)
        self.horizontalHeader().setResizeMode(1, QtGui.QHeaderView.Stretch)
```

* 首先设置该图形组件左上方的标题栏内容为“日志”（日志监控组件）
* 我们希望显示日志时，每行显示该日志的生成时间和具体的日志内容
* 由于QTableWidget本身比较类似于Excel表格，左侧有垂直标题栏（默认用于显示每行行号的表头）且可以编辑，我们需要关闭这两个功能
* 另外我们希望显示日志生成时间的列的列宽可以调整为最小（只要能看到完整的时间就行），而把显示日志内容的列设为拉升（即窗口有多宽就完全覆盖）

**注册事件监听（registerEvent）**

这里需要稍微深入一下vn.py框架中的多线程工作机制

* 整个框架在Python环境中主要包含两个线程：主线程（运行Qt循环）和事件处理线程（运行EventEngine中的工作循环）
* 针对用户是否需要使用GUI界面，主线程中运行的Qt循环可以选择QApplication（带GUI）或者QCoreApplication（纯cmd）
* Qt循环主要负责处理所有GUI相关的操作（控件绘制、信号处理等等），用户不能在其他线程中直接改变GUI界面上的任何内容，否则可能会直接导致程序崩溃
* 当用户希望在其他线程中对GUI进行操作时，必须依赖Qt提供的signal/slot机制，Qt循环的底层也运行着一个类似于EventEngine的事件处理机制，其他线程发出signal后会首先记录到一个队列中，然后由Qt对队列中的signal任务进行循环处理（具体请参考Qt相关的资料）
* 事件处理线程的工作原理在之前的教程中已经专门介绍过了，这里不再重复，用户只需记住所有的Qt GUI组件的事件处理函数，都必须使用一个signal和该函数相连，并且在向事件引擎中注册函数监听时，将该signal的emit方法代替原本的事件处理函数进行注册

```
class LogMonitor(QtGui.QTableWidget):
    """用于显示日志"""
    signal = QtCore.pyqtSignal(type(Event()))
```

* signal的创建需要放在类的构造中，而不能放在类的初始化函数中（Qt会直接报错）
* 由于事件驱动引擎在调用监听函数时会传入事件对象本身作为参数，因此在创建signal时需要允许传入一个类型为Event的参数

```
    #----------------------------------------------------------------------
    def registerEvent(self):
        """注册事件监听"""
        # Qt图形组件的GUI更新必须使用Signal/Slot机制，否则有可能导致程序崩溃
        # 因此这里先将图形更新函数作为Slot，和信号连接起来
        # 然后将信号的触发函数注册到事件驱动引擎中
        self.signal.connect(self.updateLog)
        self.__eventEngine.register(EVENT_LOG, self.signal.emit)
```

* 首先我们把signal和事件处理函数updateLog连接
* 然后贾昂signal的emit方法注册到事件驱动引擎中，监听EVENT_LOG类型的事件

**更新日志记录（updateLog）**

```
    def updateLog(self, event):
        """更新日志"""
        # 获取当前时间和日志内容
        t = time.strftime('%H:%M:%S',time.localtime(time.time()))   
        log = event.dict_['log']

        # 在表格最上方插入一行
        self.insertRow(0)

        # 创建单元格
        cellTime = QtGui.QTableWidgetItem(t)    
        cellLog = QtGui.QTableWidgetItem(log)

        # 将单元格插入表格
        self.setItem(0, 0, cellTime)            
        self.setItem(0, 1, cellLog)
```

代码逻辑很清楚，无需赘言

# 总结

尽管Qt库本身使用C++开发，相比之下在Python中使用PyQt构建GUI更为快捷、简便。基于Python的动态语言的特性，在很多不是特别追求性能（GUI更新速度）的地方可以大幅减少用户的代码编写量，并且降低出错率

请记住vn.py从开始就是一款专门为交易员设计的通用型交易平台开发框架（而不止是全自动的程序化交易），在金融市场上真正能帮助交易员赚钱的绝对不是多么复杂的程序算法，而是能够完美实现交易眼的交易策略并且越简单越好的工具

# 我的思考

是不是直接可以做一个Web版本的界面，使用HTML、JavaScript、CSS感觉功能会更强大，也不要再花时间浪费在学习PyQt上

不过，用HTML、JavaScript、CSS熟练的做出来简洁、清晰的界面，难度还是很大，毕竟Web前端也真得是有比较大的难度
