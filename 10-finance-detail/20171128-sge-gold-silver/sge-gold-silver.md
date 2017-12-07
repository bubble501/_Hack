## 背景介绍

上海黄金交易所业务是我来到恒生之后做的第一个比较大型的、完整的业务领域，我当然是主要负责其中的报盘部分开发。但是整个开发的过程中我做的事情很简单，就是对照着接口文档对所有需要的字段进行打包解包即可，完全没有思考整个交易流程、没有思考每个字段的深入的用处、没有将详细的交易规则梳理清楚、没有将资金和标的的流转逻辑梳理清楚……

之前我也在自己参考一些简单的资料，在自己的博客[《金银天然不是货币，但货币天然是金银》](http://www.xumenger.com/gold-trade-20170310/)一文中对贵金属业务做过简单整理，但明显不清晰、不详细

当然，因为自己已经接触黄金业务多年，所以还是可以提出一些建设性的可以指明接下来学习方向的问题：

* 上海黄金交易所的历史、管理架构……
* 黄金业务的账户结构详细是什么样子的？
* 贵金属的各种衍生品是基于什么思想设计和产生的？
* 铜、银、金、铂金等不同贵金属在金融业务上有什么不同？
* 黄金即期业务是做什么的？
* 黄金延期业务是做什么的？
* 黄金延期交易和黄金期货有很多类似的地方，但它们根本的区别是什么？
* 为什么要存在黄金TD、白银TD这种业务？
* 黄金仓储是做什么用的？
* 黄金ETF的投资端要通过什么途径投资？
	* 做沪深业务的时候都有黄金ETF的业务品种
	* 不过ETF既能申赎又能买卖，这个点需要考虑到
* 黄金ETF的发行端要在哪里发行？发行的规则是什么样的？
* 对于黄金业务的收费问题
	* 如果每天频繁的交易，那么建仓费、平仓费会增加交易成本
	* 如果长期持仓的话，每天都要计算的持仓隔夜费也会增加交易成本
	* 所以如何在复杂的收费模式下通过投资黄金获利，还是很有难度的事情
	* 投资者必须事先了解好交易规则，能熟练的计算收益、费用等重要的指标
* T+0和T+N的不同要求对于投资者决定投资策略有什么影响？
* 国内个人投资者有哪些可以投资黄金业务的途径？
* 为什么有的业务有涨跌幅限制，为什么有的业务没有这个限制？

>在[《金银天然不是货币，但货币天然是金银》](http://www.xumenger.com/gold-trade-20170310/)整理过的内容，不会再在这里重复输入，下面的整理将是根据我在工作中接触的内容来做的整理，更贴近我实际的工作内容和工作经验

另外，上海黄金交易所的网址是[http://www.sge.com.cn/](http://www.sge.com.cn/)，可以在官网上找到一些业务文档：

* [黄金大讲堂](http://www.sge.com.cn/tzzjy/hjdjt)
* [交易指南](http://www.sge.com.cn/tzzjy/jyzy)

>一般交易所的官网上都会有很多资料，可以帮助我们对业务进行学习和梳理

## 竞价交易

对接金交所竞价交易平台，通过交易员进行登录

* 一个席位下面有多个交易员
* 一个席位下面有多个客户
* 客户和交易员之间没有直接关系

## ETF投资端

对接金交所ETF平台，通过席位进行登录。根据会员代码与席位属性生成，规则如下：

* 自营席位：会员代码 + 01
* 代理个人席位：会员代码 + 02
* 代理法人席位：会员代码 + 03

客户绑定/解绑定的流程是这样的：

* T日下单和报单流程：
	* 前台下单是未报
	* 转换机取出来是待报
	* 转换机发送出去是正报
	* 收到交易所的应答是已报
* 是否绑定成功的流程是这样的：
	* 基金公司将结果给金交所
	* 金交所通过下发清算文件方式给二级系统
* T日晚，清算文件下发“701未绑定”的信息
* “702绑定成功”、“703绑定失败”下发规则：
	* 在第二天开始的任一天下发
	* 绑定成功后可交易
	* 具体的下发日期根据交易所是否绑定成功
	* 业务上有可能一年都无绑定成功或失败通知

>交易所允许进行认申赎的时间段：9:30~11:30、13:30~15:00

## ETF发行端

客户端绑定/解绑定的流程是这样的：

* 投资端发起账户绑定请求
* 交易所把投资端的账户绑定请求通知发行端
* 是否绑定成功是基金公司给金交所的
	* 金交所通过下发清算文件的方式给投资端
	* 702绑定成功、703绑定失败，在第二天开始的任何一天下发
	* 绑定成功后可进行ETF认购申购交易

## 仓储

????????????????????????????????????

## 参考资料

* [《黄金大讲堂：黄金市场发展与黄金投资》](http://www.sge.com.cn/tzzjy/hjdjt/521447)
* 《黄金交易需求文档》
* [《黄金白银投资一点通》](http://www.sge.com.cn/upload/resources/file/2015/02/27/29598.pdf)
* 金交所的【易金通】APP上面也有不少值得参考的资料