>[Python量化交易平台开发教程系列2-类CTP交易API的Python封装设计](http://www.vnpy.org/basic-tutorial-2.html)

本篇教程包含的内容太多也太复杂，有不少读者反应看不懂，因为本身也不是使用vn.py必须掌握的知识，这篇教程暂时处于半完成状态，等多收集写读者的建议后再做一个比较大的修订

# 为什么要封装API

直接原因就是C++的API没法直接在Python中使用，不过这个回答有点太简单，这里我们稍微做一些拓展解释：

* C++ API中很多函数的调用参数是ApiStruct.h（参见上一篇）中定的结构体，而在Python中我们既无法直接创建这些结构体（主动函数），也无法提取结构体中包含的数据（回调函数）
* Python虚拟机是基于C语言实现的，所有的Python对象，哪怕只是一个整数或者字符串，在C的环境中都是一个PyObject对象（好吧，我知道C中没有对象，只有结构体，但估计90%的读者都不在乎这个区别）。用户如果在Python中直接传递一个参数到C++环境中，C++是无法识别的（Python：买入一手股指，C++：你要买入多少？）
* Python只能加载封装为PyObject对象的模块，因此原生C++的API在Python中连加载都加载不了

# 封装后API的工作流程

### 主动函数

* 用户在Python程序中调用封装API的主动函数，并直接传入Python变量（PyObject对象）作为参数
* 封装API将Python变量转换成C++变量
* 封装API调用原生API的主动函数，并传入C++变量作为参数

### 回调函数

* 交易柜台通过原生API的C++回调函数推送数据信息，传入参数为C++变量
* 封装API将C++变量转换为Python变量
* 封装API调用封装后的回调函数向用户的Python程序这种推送数据，传入参数为Python变量

### 名词定义

* 封装API：指的是经过封装后，可以直接在Python使用的API
* 原生API：指的是由软件公司提供，在C++中使用的API
* Python变量：包含Python中的数字、字符串、对象等等
* C++变量：包含C++中的内置数据类型和结构体等

# 从Python的角度看原生API的一些问题

上一篇教程后，读者应该对C++ API的结构和使用方法有了基础的了解，这篇教程主要介绍的是对原生的C++ API进行Python封装时的设计和思路，这些构成了vn.py开源项目中vn.lts（华宝证券LTS柜台API封装）的基础。首先让我们来从Python的角度看看原生API的一些问题：

* 原生的API中每个功能分为了两个类：分别是包含回调函数的Spi类和主动函数的Api类，这种设计能让用户更好的分清不同的功能。但是从面向对象的角度，把两个类封装在一起更加方便，实际使用中绝大部分C++的用户也会将接口整合到一个类里面（可以参见网上很多CTP开发的示例代码），因此Python的API中，我们也会讲Spi和Api两个类的功能封装在一个类中
* 原生的API中回调函数被触发后必须快速返回，否则会导致其他数据的推送被阻塞，阻塞时间长了还有可能导致API发生崩溃，因此回调函数中不适合包含耗时较长的计算逻辑。例如某个TICK行情推送后，如果用户在回调函数中写了一些比较复杂的计算（循环计算等），耗时超过3秒（这个数字只是笔者的一个经验），则在这个3秒中，其他的行情推送用户是收不到的（被阻塞了），且很可能3秒后会出现API崩溃（程序死掉）。这里的解决方案是使用生产者-消费者模型，在API中包含一个缓冲队列，当回调函数收到新的数据信息时只是简单存入缓冲队列中并立即返回，而数据信息的处理和向Python中的推送则由另一个工作线程来执行
* API的函数中使用了大量的结构体用于数据传送，这对C++而言是非常自然的设计，但是对Python封装会造成不小的麻烦，所有的结构体都要封装成对应的Python类，工作量太大也非常容易出错。这点我们可以利用Python相对于C++更为高级的数据结构来解决，Python中的dict字段本质是一个哈希表，但是同一个字典内建和值的类型允许不同，这个特性使得字典可以非常方便的用来代替C++的结构体

明确了以上的问题后，我们就可以开始着手设计Python API的结构了

# Python API的结构设计

这里使用行情API作为示例（...表示省略的代码）

```
//API的继承实现
class MdApi : public CSecurityFtdcMdSpi
{
private:
    CSecurityFtdcMdApi *api;          //api对象
    thread *task_thread;              //工作线程向Python中推送数据
    ConcurrentQueue<Task> task_queue; //任务队列
public:
    MdApi(){
        function<void> f = boost::bind(&MdApi::processTask, this);
        thread t(f);
        this->task_thread = &t;
    };
    ~MdApi(){};

    ...

    //登录请求响应
    virtual void OnRspUserLogin(CSecurityFtdcRspUserLoginField *pRspUserLogin, CSecurityFtdcRspInfoField *pRspInfo, int nRequestID, bool bIsLast);

    ...

    //数据任务处理函数（在工作线程中运行）
    void processTask();

    ...

    //处理登录请求响应
    void processRspUserLogin(Task task);

    ...

    //该回调函数在Python中继承
    virtual void onRspUserLogin(dict data, dict error, int id, bool last){};

    ...

    //请求登录的主动函数
    int reqUserLogin(dict req, int nRequestID);

    ...
}
```

以上代码截取自vnltsmd.h文件

注意原生API的函数名开头都是大写字母，为了便于分辨以及符合Python的PEP8编码规则，作者的函数都以小写字母开头。上面的代码中采取的示例是用户登录UserLogin这个功能

### 封装中的类和函数命名规则

* 封装后的Python API的类取名为MdApi，注意这个不是原生API中的CSecurityFtdcMdApi
* 原生API中以On开头的回调函数（如OnRspUserLogin）对应的Python API的回调函数直接改为以on开头（如onRspUserLogin）
* 原生API中的主动函数（如ReqUserLogin）对应的封装后的API中的主动函数改为首字母小写（如ReqUserLogin）

### MdApi的成员变量

* api：原生API中的CSecurityFtdcMdApi对象，用于实现主动函数的调用
* task_thread：一个boost线程指针，用于实现任务线程的工作
* task_queue：一个线程安全的任务队列

### 工作步骤（后面会有具体函数的实现细节）

* 用户在Python中调用reqUserLogin函数，传入参数为包含登录信息（用户名、密码）的字典req以及本次请求号nRequestID，该函数自动将字典中的信息提取并创建原生API使用的结构体后，调用原生API的主动函数ReqUserLogin来进行登录
* 登录成功后，原生API会调用OnRspUserLogin的回调函数返回登录信息（注意这里On是大写），在回调函数中，只是简单地把结构体数据保存到一个任务对象Task中，并推送到任务队列中
* 工作线程中运行的函数是processTask，该函数负责检查任务队列中是否有新的任务，如果有则调用对应的process函数进行处理，如果没有则阻塞等待
* processTask函数检查到任务队列中的OnRspUserLogin推送的一个任务后，调用processRspUserLogin函数进行处理。该函数首先从结构体中提取数据并转换为Python字典，然后调用OnRspUserLogin函数（这里的on是小写）推送到Python环境中，onRspUserLogin函数由用户在Python中继承实现

# Python API的函数实现

仍然使用之前的示例进行函数实现的讲解，包括MdApi的构造、析构函数、主动函数（reqUserLogin等），原生API回调函数（OnRspUserLogin等）和任务处理函数（processTask和processRspUserLogin等）

### 构造、析构函数

```
MdApi()
{
    function0<void> f = boost::bind(&MdApi::processTask, this);
    thread t(f);
    this->task_thread = &t;
};

~MdApi()
{
};
```

构造函数中仅包含了创建一个工作函数processTask的工作线程，并将该线程的指针绑定到task_thread上

析构函数为空，用户在退出前应当主动调用安全退出函数（参见源代码中的exit）

### 主动函数

```
int MdApi::reqUserLogin(dict req, int nRequestID)
{
    //创建原生API函数调用需要的结构体
    CSecurityFtdcReqUserLoginField myreq = CSecurityFtdcUserLoginField();

    //初始化这个结构体的内存
    memset(&myreq, 0, sizeof(myreq));

    //提取字典中的内容并复制到结构体中
    getChar(req, "MacAddress", myreq.MacAddress);
    getChar(req, "UserProductInfo", myreq.UserProductInfo);
    getChar(req, "UserID", myreq.UserID);
    getChar(req, "AuthCode", myreq.AuthCode);
    getChar(req, "TradingDay", myreq.TradingDay);
    getChar(req, "InterfaceProductInfo", myreq.InterfaceProductInfo);
    getChar(req, "BrokerID", myreq.BrokerID);
    getChar(req, "ClientIPAddress", myreq.ClientIPAddress);
    getChar(req, "OneTimePassword", myreq.OneTimePassword);
    getChar(req, "ProtocolInfo", myreq.ProtocolInfo);
    getChar(req, "Password", myreq.Password);

    //将结构体的指针和代表请求编号的整数作为参数调用原生API的函数
    int i = this->api->ReqUserLogin(&myreq, nRequestID);

    //返回原生API函数的调用结果
    return i;
}
```

原生API中的请求登录函数为ReqUserLogin，传入的参数一共包含两个：一个CSecurityFtdcReqUserLoginField结构体的指针，一个代表请求编号的整数

封装后的API函数为reqUserLogin，传入参数同样为两个：一个Python字典对象、一个整数。reqUserLogin函数会从Python字典对象中根据键值依次提取结构体中对应的数据。如结构体中有一个成员叫做BrokerID，则使用getChar函数从字段对象中提取“BrokerID”键对应的值

getChar函数的实现如下：

```
// d为Python字典对象
// key为d中想要提取的数据的键名
// value为最终需要这个数据的结构体成员的指针
void getChar(dict d, string key, char *value)
{
    //首先检查字典中是否存在key这个键
    if (d.has_key(key)) {
        //提取key这个键对应的值，即Python对象o
        object o = d[key];

        //生成从o中提取std::string类的提取器
        extract<string> x(o);

        //检查提取器是否能提取出数据
        if (x.check()) {
        	//执行解包器，提取string对象s
        	string s = x();

        	//从s中获取字符串指针buffer
        	const char *buffer = s.c_str();

        	//字符串指针指向的字符串数组复制到结构体成员的指针上
        	//对字符串指针复制使用strcpy_s, vs2013使用strcpy编译通不过
            //+1应该是因为C++字符串的结尾符号？不是特别确定，不加这个1会出错
            strcpy_s(value,  strlen(buffer) + 1, buffer);
        }
    }
}
```

由于原生API中用到的底层数据类型主要包括四种：char字符、char[]字符串数组、int整型、double浮点数，可以对应的Python中的数据类型为：string、int、float。因此设计了三个函数getChar、getInt、getDouble来从Python对象中提取所需的C++数据，getInt、getDouble请参见源代码

### 原生API的回调函数

```
//登录应答回调函数
void MdApi::OnRspUserLogin(CSecurityFtdcRspUserLoginField *pRspUserLogin, CSecurityFtdcRspInfoField *pRspInfo, int nRequestID, bool bIsLast)
{
    Task task = Task();
    task.task_name = ONRSPUSERLOGIN;
    task.task_data = *pRspUserLogin;
    if (pRspInfo) {
        task.task_error = *pRspInfo;
    } else {
        CSecurityFtdcRspInfoField empty_error = CSecurityFtdcRspInfoField();
        memset(&empty_error, 0, sizeof(empty_error));
        task.task_error = empty_error;
    }
    task.task_id = nRequestID;
    task.task_last = bIsLast;
    this->task_queue.push(task);
}
```

当登录成功后，原生API中的回调函数OnRspUserLogin会被自动调用，通知用户登录相关的信息，传入参数包括四个，分别为CSecurityFtdcRspUserLoginField结构体指针（用户本次登录的相关信息）pRspUserLogin，CSecurityFtdcRspInfoField结构体指针（登录是否存在错误的相关信息）pRspInfo，整数（登录请求编号）nRequestID和布尔值（是否为该请求的最后一次通知）bIsLast

在回调函数中，我们通过创建一个Task对象来保存这些信息，并推入task_queue中，等待工作线程的提取处理。其中，由于pRspInfo可能存在空指针的情况，所以需要进行判断，若指针为空，则在Task对象上绑定一个内容为空的CSecurityFtdcRspInfoField结构体（这步等于一个异常情况的处理）。ONRSPUSERLOGIN是一个整型常量（在头文件中定义），用于标识该Task对象包含的是哪个回调函数返回的信息

Task对象的定义如下：

```
//任务结构体
struct Task
{
    int task_name;      //回调函数名称对应的常量
    any task_data;      //数据结构体
    any task_error;     //错误结构体
    int task_id;        //请求ID
    bool task_last;     //是否为最后返回
};
```

其中any是boost库中的any类，作用是定义一个可以存放任意类型数据的变量（这有点类似于Python中的变量），但是当用户尝试从该变量中获取原本的数据时，需要知道原本数据的类型。原生API中不同回调函数返回的参数类型是不同的，因此为了提高代码的简洁性选择使用boost.any这个泛型值

### 任务处理函数

首先是负责从任务队列中提取任务，并根据任务名称的不同使用对应的函数进行处理的processTask函数

```
/**
工作线程从队列中取出数据，转化为Python对象后，进行推送
**/
void MdApi::processTask()
{
    while (1) {
        Task task = this->task_queue.wait_and_pop();
        switch (task.task_name){
            ...

            case ONRSPUSERLOGIN:
            {
                this->processRspUserLogin(task);
                break;
            }

            ...
        };
    }
};
```

使用while(1)的方式让processTask处于无限循环中不断运行，从task_queue队列中提取任务对象task后，使用switch根据任务的回调函数名称task_name，调用对应的函数处理该任务。上面的例子中，当程序检查task_name是ONRSPUSERLOGIN这个常量值后，就会调用processRspUserLoin函数进行处理，其代码如下

```
void MdApi::processRspUserLogin(Task task)
{
    CSecurityFtdcRspUserLoginField task_data = any_cast<CSecurityFtdcRspUserLoginField>(task.task_data);
    
    dict data;
    data["MaxOrderRef"] = task_data.MaxOrderRef;
    data["UserID"] = task_data.UserID;
    data["TradingDay"] = task_data.TradingDay;
    data["SessionID"] = task_data.SessionID;
    data["SystemName"] = task_data.SystemName;
    data["FrontID"] = task_data.FrontID;
    data["BrokerID"] = task_data.BrokerID;
    data["LoginTime"] = task_data.LoginTime;

    CSecurityFtdcRspInfoField task_error = any_cast<CSecurityFtdcRspInfoField>(task.task_error);
    dict error;
    error["ErrorMsg"] = task_error.ErrorMsg;
    error["ErrorID"] = task_error.ErrorID;

    this->onRspUserLogin(data, error, task.task_id, task.task_last);
}
```

any_cast函数由boost.any库提供，作用之前提到的any变量中提取出用户需要的数据类型来。dict类有boost.python库提供，使用dict可以直接创建Python环境中的字典，同时当我们使用d[key] = value这种语句进行赋值时，dict中的key和value均会自动转换为对应的Python对象。当我们将返回的业务信息CSecurityFtdcRspUserLoginField结构体和错误信息结构体CSecurityFtdcRspInfoField分别转换为data和error这两个Python字典后，我们就可以通过OnRspUserLogin回调函数推送到Python环境中了

# 总结

之前几段示例代码展示的是用户登录这个简单业务操作，包括了从用户在Python中调用主动函数到柜台通过回调函数返回信息再推送到Python中的全过程。文章主要是对源代码中的注释起到一个更为细致的解释作用。同样这篇内容对于想用vn.py做量化平台开发的用户而言不是必须掌握的东西，放在这里主要是考虑教程的完整性，看不懂的就先跳过吧

下一章是vn.py平台中API部分的编译方法，github上项目中的.pyd文件可能由于你的操作系统或者编译器和作者本人的不同而没法直接使用，必须自行编译，整个教程会包含一步步的截图和说明

>这部分和我在编写黄金交易所的报盘任务中的逻辑是极其相似的，所以看起来简直就是如鱼得水，唯一的问题就是后续可能需要针对性的整理Python如何与C++配合开发？！