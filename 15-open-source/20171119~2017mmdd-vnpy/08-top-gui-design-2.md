[Python量化交易平台开发教程系列8-顶层GUI界面开发（2）](http://www.vnpy.org/basic-tutorial-8.html)

# 前言

接上一篇，主要分为两块：展示动态语言特性简化GUI开发的组件以及功能调用组件

# 动态语言的方便之处

```
class AccountMonitor(tGui.QTableWidget):
    """用于显示账户"""
    signal = QtCore.pyqtSignal(type(Event()))

    dictLabels = OrderedDict()
    dictLabels['AccountID'] = u'投资者账户
    dictLabels['PreBalance'] = u'昨结'
    dictLabels['Withdraw'] = u'出金'
    dictLabels['Deposit'] = u'入金'
    dictLabels['FrozenCash'] = u'冻结资金'
    dictLabels['FrozenMargin'] = u'冻结保证金'
    dictLabels['ForzenCommission'] = u'冻结手续费'
    dictLabels['FrozenTransferFee'] = u'冻结过户费'
    dictLabels['FrozenStampTax'] = u'冻结印花税'
    dictLabels['Commission'] = u'手续费'
    dictLabels['TransferFee'] = u'过户费'
    dictLabels['StampTax'] = u'印花税'
    dictLabels['CurrMargin'] = u'当前保证金'
    dictLabels['Available'] = u'可用资金'
    dictLabels['WithdrawQuota'] = u'可取资金'

    def __init__(self, eventEngine, parent=None):
        super(AccountMonitor, self).__init__(parent)
        self.__eventEngine = eventEngine

        self.dictAccount = {}    # 用来保存账户对应的单元格
        self.initUi()
        self.registerEvent()

    def initUi(self):
        self.setWindowTitle(u'账户')

        self.setColumnCount(len(self.dictLabels))
        self.setHorizontalHeaderLabels(self.dictLabels.values())

        self.verticalHeader().setVisible(False)                 # 关闭左边的垂直表头
        self.setEditTriggers(QtGui.QTableWidget.NoEditTriggers) # 设为不可编辑状态

    def registerEvent(self):
        self.signal.connect(self.updateAccount)
        self.__eventEngine.register(EVENT_ACCOUNT, self.signal.emit)

    def updateAccount(self, event):
        data = event.dict_['data']
        accountid = data['AccountID']

        # 如果之前已经收到过这个账户的数据，则直接更新
        if accountid in self.dictAccount:
            d = selff.dictAccount[accountid]

            for label, cell in d.items():
                cell.setText(str(data[label]))
        # 否则插入新的一行，并更新
        else:
            self.insertRow(0)
            d = {}
            for col, label in enumerate(self.dictLabels.keys()):
                cell = QtGui.QTableWidgetItem(str(data[label]))
                slef.setItem(0, col, cell)
                d[label] = cell

            self.dictAccount[accountid] = d
```

写过C++里Qt代码的人可能会有比较直观的感受，上面这段代码利用Python动态语言的特性偷了不少懒

该账户监控组件AccountMonitor用于显示LTS账户相关的数据（可用资金、手续费、保证金等）。在vn.lts的API封装中，通过onRspQryTradingAccount推送回来的账户数据已经被从C++结构体自动转换为Python字典

```
Class AccountMonitor(tGui.QTableWidget):
    """用于显示账户"""
    signal = QtCore.pyqtSignal(type(Event()))

    dictLabels = OrderedDict()
    dictLabels['AccountID'] = u'投资者账户
    dictLabels['PreBalance'] = u'昨结'
    dictLabels['Withdraw'] = u'出金'
    dictLabels['Deposit'] = u'入金'
    dictLabels['FrozenCash'] = u'冻结资金'
    dictLabels['FrozenMargin'] = u'冻结保证金'
    dictLabels['ForzenCommission'] = u'冻结手续费'
    dictLabels['FrozenTransferFee'] = u'冻结过户费'
    dictLabels['FrozenStampTax'] = u'冻结印花税'
    dictLabels['Commission'] = u'手续费'
    dictLabels['TransferFee'] = u'过户费'
    dictLabels['StampTax'] = u'印花税'
    dictLabels['CurrMargin'] = u'当前保证金'
    dictLabels['Available'] = u'可用资金'
    dictLabels['WithdrawQuota'] = u'可取资金'
```

首先通过原生的API的.h头文件查询该结构体包含的字段内容，并选择觉得有用、需要显示出来的字段，使用Python内置的排序字典类OrderedDict创建标签显示的配置dictLabels，按照我们希望显示的顺序（从左到右）逐条输入到该字典中。字典的键是该字段的英文名称，值是该字段对应的中文名称

```
    def initUi(self):
        """"""
        self.setWindowTitle(u'账户')

        self.setColumnCount(len(self.dictLabels))
        self.setHorizontalHeaderLabels(self.dictLabels.values())
```

下一步在初始化表格时，表头的列数可以直接取dictLabels的长度，表头的标签内容可以直接取dictLabels的值列表

```
    def updateAccount(self, event):
        """"""
        data = event.dict_['data']
        accountid = data['AccountID']

        # 如果之前已经收到过这个账户的数据, 则直接更新
        if accountid in self.dictAccount:
            d = self.dictAccount[accountid]

            for label, cell in d.items():
                cell.setText(str(data[label]))
        # 否则插入新的一行，并更新
        else:
            self.insertRow(0)
            d = {}

            for col, label in enumerate(self.dictLabels.keys()):
                cell = QtGui.QTableWidgetItem(str(data[label]))
                self.setItem(0, col, cell)
                d[label] = cell

            self.dictAccount[accountid] = d
```

在更新账户数据时，由于dictLabels中设置的需要显示的字段和收到的账户数据字典data中的键是一一对应的，我们可以直接对dictLabels中的键进行遍历读取data字典中对应的值，并更新到表格中。这段感觉比较难以用语言描述出来，直接读懂上面的代码可能更能明白其中的意思

可以来比较下啊updateAccount函数的类C++的写法，也是作者在刚开始做GUI开发时使用的方法，现在回看起来真是繁琐

```
    def updateAccount(self, event):
        """"""
        data = event.dict_['data']
        accountid = data['AccountID']

        # 如果之前已经收到过这个账户的数据, 则直接更新
        if accountid in self.dictAccount:
            d = self.dictAccount[accountid]

            d['AccountID'].setText(str(data['AccountID']))
            d['PreBalance'].setText(str(data['PreBalance']))
            d['Deposit'].setText(str(data['Deposit']))
            d['FrozenCash'].setText(str(data['FrozenCash']))
            d['FrozenMargin'].setText(str(data['FrozenMargin']))
            d['FrozenCommission'].setText(str(data['FrozenCommission']))
            d['FrozenTransferFee'].setText(str(data['FrozenTransferFee']))
            d['FrozenStampTax'].setText(str(data['FrozenStampTax']))
            d['Commission'].setText(str(data['Commission']))
            d['TransferFee'].setText(str(data['TransferFee']))
            d['StampTax'].setText(str(data['StampTax']))
            d['CurrMargin'].setText(str(data['CurrMargin']))
            d['Available'].setText(str(data['Available']))
            d['WithdrawQuota'].setText(str(data['WithdrawQuota']))

        # 否则插入新的一行，并更新
        else:
            self.insertRow(0)
            d = {}

            cellAccountID = QtGui.QTableWidgetItem(str(data['AccountID']))
            self.setItem(0, 0, cellAccountID)
            d['AccountID'] = cell

            cellPreBalance = QtGui.QTableWidgetItem(str(data['PreBalance']))
            self.setItem(0, 0, cellPreBalance)
            d['PreBalance'] = cell

            ...
            （后面的字段以此类推，这里请原谅作者实在不想写下去了，习惯遍历后复制粘贴都觉得麻烦）

            self.dictAccount[accountid] = d
```

和上面的类C++写法相比，之前Python写法的优势就很明显了

* 代码行数显著减少（绝大部分都是重复内容），降低了coding出错的概率，省下很多debug的时间
* dictLabels可以直接作为配置来使用（类似于Java中使用很多配置文件），当需要增加或者移除某个字段时，用户只需要更改dictLabels中的内容而无需改变initUi和updateAccount（这才是浪费无数debug时间的地方），同时修改的配置和本身的程序代码都一样是Python源代码文件（不像Java使用XML等其他文件进行配置）

当然也存在一个缺点：理论上list遍历的使用降低了性能（GUI的更新耗时更长），但是作者以一个实际用户的经验可以告诉大家，这个性能上的牺牲微乎其微，考虑到vn.py的应用场景，这个缺点几乎可以忽略

# 功能调用组件

```
class LoginWidget(QtGui.QDialog):
    """登录"""

    def __init__(self, mainEngine, parent=None):
        super(LoginWidget, self).__init__()
        self.__mainEngine = mainEngine

        self.initUi()
        self.loadData()

    def initUi(self):
        """初始化界面"""
        self.setWindowTitle(u'登录')

        # 设置组件
        labelUserID = QtGui.QLabel(u'账号：')
        labelMdPassword = QtGui.QLabel(u'行情密码：')
        labelTdPassword = QtGui.QLabel(u'交易密码：')
        labelMdAddress = QtGui.QLabel(u'行情服务器：')
        labelTdAddress = QtGui.QLabel(u'交易服务器：')

        self.editUserID = QtGui.QLineEdit()
        self.editMdPassword = QtGui.QLineEdit()
        self.editTdPassword = QtGui.QLineEdit()
        self.editMdAddress = QtGui.QLineEdit()
        self.editTdAddress = QtGui.QLineEdit()

        self.editUserID.setMinimumWidth(200)

        buttonLogin = QtGui.QPushButton(u'登录')
        buttonCancel = QtGui.QPushButton(u'取消')
        buttonLogin.clicked.connect(self.login)   # 登录按钮点击回调函数
        buttonCancel.clicked.connect(self.close)

        # 设置布局
        buttonHBox = QtGui.QHBoxLayout()
        buttonHBox.addStretch()
        buttonHBox.addWidget(buttonLogin)
        buttonHBox.addWidget(buttonCancel)

        grid = QtGui.QGridLayout()
        grid.addWidget(labelUserID, 0, 0)
        grid.addWidget(labelUserID, 0, 0)
        grid.addWidget(labelMdPassword, 1, 0)
        grid.addWidget(labelTdPassword, 2, 0)
        grid.addWidget(labelMdAddress, 3, 0)
        grid.addWidget(labelTdAddress, 4, 0)
        grid.addWidget(self.editUserID, 0, 1)
        grid.addWidget(self.editMdPassword, 1, 1)
        grid.addWidget(self.editTdPassword, 2, 1)
        grid.addWidget(self.editMdAddress, 3, 1)
        grid.addWidget(self.editTdAddress, 4, 1)
        grid.addLayout(buttonHBox, 5, 0, 1, 2)

        self.setLayout(grid)

    def login(self):
        """登录"""
        userid = str(self.editUserID.text())
        mdPassword = str(self.editMdPassword.text())
        tdPassword = str(self.editTdPassword.text())
        mdAddress = str(self.editMdAddress.text())
        tdAddress = str(self.editTdAddress.text())
        brokerid = '2011'    # 这里因为LTS的BrokerID固定为2011

        self.__mainEngine.login(userid, mdPassword, tdPassword, brokerid, mdAddress, tdAddress)
        self.close()

    def loadData(self):
        """读取数据"""
        f = shelve.open('setting.vn')
        try:
        	setting = f['login']
        	userid = setting['userid']
        	mdPassword = setting['mdPassword']
            tdPassword = setting['tdPassword']
            mdAddress = setting['mdAddress']
            tdAddress = setting['tdAddress']

            self.editUserID.setText(userid)
            self.editMdPassword.setText(mdPassword)
            self.editTdPassword.setText(tdPassword)
            self.editMdAddress.setText(mdAddress)
            self.editTdAddress.setText(tdAddress)
        except KeyError:
            pass
        f.close()

    def saveData(self):
        """保存数据"""
        setting = {}
        setting['userid'] = str(self.editUserID.text())
        setting['mdPassword'] = str(self.editMdPassword.text())
        setting['tdPassword'] = str(self.editTdPassword.text())
        setting['mdAddress'] = str(self.editMdAddress.text())
        setting['tdAddress'] = str(self.editTdAddress.text())

        f = shelve.open('setting.vn')
        f['login'] = setting
        f.close()

    def closeEvent(self, event):
        """关闭事件处理"""
        # 当窗口被关闭时，先保存登录数据，再关闭
        self.saveData()
        event.accept()
```

* 主动调用组件通常在工作原理上较为简单，用户只需要在界面上放置所需的组件（按钮、下拉框等），并将组件的信号和中层引擎暴露的功能函数连接上（参考上面的initUi和Login方法）
* 对于一些我们希望保存下来省去每次输入麻烦的信息，这里推荐Python的shelve库，可以直接将Python中的数据对象以二进制的方式保存在文件中（文件名后缀可自行定义）
* 在登录组件第一次创建时，我们从硬盘上的setting.vn文件中尝试读取登录账号等设置内容，同时每次在关闭该窗口时closeEvent方法会自动触发，保存当前文本框中设置内容到文件中

# 总结

系列7、8两篇教程中已经基本覆盖了GUI界面开发的核心原理，剩下的更多细节以及开发中的一些奇技淫巧就留给读者自己去研究vn.py项目的源码了，毕竟对于这套教程的目标读者有能力开发适合自己交易策略的量化平台才是最终目标：now it's time to get your hands dirty
