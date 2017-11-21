>[Python量化交易平台开发教程系列6-中层引擎设计](http://www.vnpy.org/basic-tutorial-6.html)

# 前言

中层引擎在设计上主要是为了进一步封装接口欧所暴露出的API函数，使得其更容易被上层的GUI和策略组件调用。本篇的内容会相对简单，主要以LTS接口DEMO为例介绍一些设计方面的思路

相关的示例都是基于vn.demo中的LTS接口DEMO，发布在[https://github.com/vnpy/vnpy/tree/master/vn.demo/ltsdemo](https://github.com/vnpy/vnpy/tree/master/vn.demo/ltsdemo)

# 中间引擎设计

### 构造函数

```
class MainEngine:
    """主引擎，负责对API的调度"""
    
    #----------------------------------------------
    def __init__(self):
        self.ee = EventEngine()       # 创建事件驱动引擎

        self.md = DemoMdApi(self.ee)  # 创建API接口
        #self.md = DemoL2Api(self.ee) # 如果使用L2行情就改为这行
        self.td = DemoTaApi(self.ee)

        self.ee.start()               # 启动事件驱动引擎

        # 循环查询持仓和账户相关
        self.countGet = 0             # 查询延时计数
        self.lastGet = 'Account'      # 上次查询的性质
        self.ee.register(EVENT_TDLOGIN, self.initGet)   # 登录成功后开始初始化查询

        # 合约储存相关
        self.dictInstrument = {}      # 字典（保存合约查询数据
        self.ee.register(EVENT_INSTRUMENT, self.insertInstrument)
```

1. 首先以主引擎成员变量的形式创建事件驱动引擎ee、行情接口md和交易接口td的对象，两个接口对象创建时传入事件驱动引擎ee作为构造函数的参数
2. 在创建以上三个对象后，立即启动事件驱动引擎，此后当用户调用接口的连接、登录等功能收到事件推送后，ee可以立即推送到监听这些事件的组件进行处理
3. LTS和CTP接口的持仓情况和账户情况并不会通过回调函数主动推送，只有在投资者调用查询函数时才会返回，我们的DEMO作为一个手动交易终端。选择使用循环查询的模式不断获取持仓和账户情况的更新。这里的每次查询都会占用网络带宽导致系统延时的增加，对于某些运行全自动策略的程序，可以选择不进行查询，而通过对成交、行情等数据的统计来自行计算持仓和账户情况（甚至对某些策略可以直接忽略该步骤，进一步降低延时水平）
4. LTS和CTP发送主动查询指令时，存在流量控制（通常限制在1s一次查询），因此选择间隔发送查询账户和查询持仓的指令
5. 在登录成功后，我们需要查询柜台上所有可交易的合约信息，将这些信息保存到字典dictInstrument中，方便后面需要时进行查询。这里很好的体现出了Python的方便，笔者在设计封装API的回调函数时特别选择使用Python字典的形式推送信息，包含合约信息的字典收到后，可以直接插入到dictInstrument字典进行保存（字典嵌字典），查询时直接使用合约代码即可。而用C++语言开发时通常还需要用到Sqlite之类的内存数据库进行保存，麻烦了不少

### 其他主动函数

```
    #----------------------------------------------------------------------
    def login(self, userid, mdPassword, tdPassword, brokerid, mdAddress, tdAddress):
        """登陆"""
        self.md.login(mdAddress, userid, mdPassword, brokerid)  //行情
        self.td.login(tdAddress, userid, tdPassword, brokerid)  //交易

    #----------------------------------------------------------------------
    def subscribe(self, instrumentid, exchangeid):
        """订阅合约"""
        self.md.subscribe(instrumentid, exchangeid)

    #----------------------------------------------------------------------
    def getAccount(self):
        """查询账户"""
        self.td.getAccount()

    #----------------------------------------------------------------------
    def getInvestor(self):
        """查询投资者"""
        self.td.getInvestor()

    #----------------------------------------------------------------------
    def getPosition(self):
        """查询持仓"""
        self.td.getPosition()

    #----------------------------------------------------------------------
    def getInstrument(self):
        """获取合约"""
        event = Event(type_=EVENT_LOG)
        log = u'查询合约信息'
        event.dict_['log'] = log
        self.ee.put(event)

        self.td.getInstrument()

    #----------------------------------------------------------------------
    def sendOrder(self, instrumentid, exchangeid, price, pricetype, volume, direction, offset):
        """发单"""
        self.td.sendOrder(instrumentid, exchangeid, price, pricetype, volume, direction, offset)

    #----------------------------------------------------------------------
    def cancelOrder(self, instrumentid, exchangeid, orderref, frontid, sessionid):
        """撤单"""
        self.td.cancelOrder(instrumentid, exchangeid, orderref, frontid, sessionid)

    #----------------------------------------------------------------------
    def getAccountPosition(self, event):
        """循环查询账户和持仓"""
        self.countGet = self.countGet + 1

        # 每5秒发一次查询
        if self.countGet > 5:
            self.countGet = 0   # 清空计数

            if self.lastGet == 'Account':
                self.getPosition()
                self.lastGet = 'Position'
            else:
                self.getAccount()
                self.lastGet = 'Account'

    #----------------------------------------------------------------------
    def initGet(self, event):
        """在交易服务器登录成功后，开始初始化查询"""
        # 打开设定文件setting.vn
        f = shelve.open('setting.vn')

        # 尝试读取设定字典，若该字典不存在，则发出查询请求
        try:
            d = f['instrument']

            # 如果本地保存的合约数据是今日的，则载入，否则发出查询请求
            today = date.today()
            if d['date'] == today:
                self.dictInstrument = d['dictInstrument']

                event = Event(type_=EVENT_LOG)
                log = u'合约信息读取完成'
                event.dict_['log'] = log
                self.ee.put(event)

                self.getInvestor()

                # 开始循环查询
                self.ee.register(EVENT_TIMER, self.getAccountPosition)                 
            else:
                self.getInstrument()
        except KeyError:
            self.getInstrument()

        f.close()

    #----------------------------------------------------------------------
    def insertInstrument(self, event):
        """插入合约对象"""
        data = event.dict_['data']
        last = event.dict_['last']

        self.dictInstrument[data['InstrumentID']] = data

        # 合约对象查询完成后，查询投资者信息并开始循环查询
        if last:
            # 将查询完成的合约信息保存到本地文件，今日登录可直接使用不再查询
            self.saveInstrument()

            event = Event(type_=EVENT_LOG)
            log = u'合约信息查询完成'
            event.dict_['log'] = log
            self.ee.put(event)

            self.getInvestor()

            # 开始循环查询
            self.ee.register(EVENT_TIMER, self.getAccountPosition)

    #----------------------------------------------------------------------
    def selectInstrument(self, instrumentid):
        """获取合约信息对象"""
        try:
            instrument = self.dictInstrument[instrumentid]
        except KeyError:
            instrument = None
        return instrument

    #----------------------------------------------------------------------
    def exit(self):
        """退出"""
        # 销毁API对象
        self.td = None
        self.md = None

        # 停止事件驱动引擎
        self.ee.stop()

    #----------------------------------------------------------------------
    def saveInstrument(self):
        """保存合约属性数据"""
        f = shelve.open('setting.vn')
        d = {}
        d['dictInstrument'] = self.dictInstrument
        d['date'] = date.today()
        f['instrument'] = d
        f.close()
```

1. LTS和CTP的行情和交易账户通常是由经纪商一起提供的，为了简洁起见，把两个接口的登录一起封装在login中
2. subscribe、getAccount、getPosition、getInvestor、getInstrument、sendOrder、cancelOrder都只是直接调用行情和交易接口的功能，做这种设计的目的是为了让用户在写上层组件代码时，可以直接写self.mainEngine/subscribe('IF1506', 'CFFEX')，而不必去写self.mainEngine.md.subscribe('IF1506', 'CFFEX')，很小的区别，但是相信我，程序规模大了后会让你烦到崩溃
3. getAccountPosition用于实现循环查询账户和持仓，这里我设为了每5s发一次，基本够用就好
4. initGet和insertInstrument两个函数实现了登录后初始化查询相关的功能。首先登录完成后，程序用shelve模块打开本地保存的文件setting.vn，检查其中的合约信息数据，如果该数据的更新日期为今日，则直接读取使用，无需再次查询可交易合约信息（一般该信息每天日内保持不变 ）。否则调用getInstrument函数查询合约信息，insertInstrument函数负责将收到的合约信息插入到dictInstrument字典中，当所有合约信息都插入完成后，通过saveInstrument函数将该字典保存到setting.vn着呢个，即可实现日内再次登录无需查询直接读取
5. 合约信息查询完成后，通常会再查询一次投资者姓名getInvestor，主要用于输出在程序的标题栏上（方便用户检查，防止登录错账户等），并将循环查询函数getAccountPosition注册到每秒触发的定时器事件监听上，开始循环查询
6. 当用户退出程序时，需要调用主引擎的exit函数，释放行情和交易API接口对象（在C++环境中会自动析构），并停止事件驱动引擎的工作线程（否则可能包线程退出错误）

# 总结

中层引擎的设计思路主要是封装底层接口，从而让用户在开发顶层GUI和策略组件时无需直接调用底层接口的主动函数，降低开发和维护的复杂度

DEMO中的这个中层引擎非常简单，功能上只实现了循环查询（其实也可以在接口层中实现，视乎需求）。而在实际开发中，中层引擎通常会包含用户最常用的一些功能模块，比如做期权策略的会加入期权定价引擎，做期现套利的会加入持仓组合的敞口监控对冲引擎等等
