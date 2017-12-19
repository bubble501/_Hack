## 前言

最近在开发一个小型的量化交易系统，我目前被分配的任务就是利用Datadog、阿里云平台开发一个小的监控系统

* 在量化交易系统中使用datadog.statsd的接口进行代码埋点
* 埋点代码收集到的监控数据会发送到Datadog云平台
* 在Datadog平台对Metric进行监控，绑定到对应的webhook上
* 比如当某个监控的指标异常时，Datadog会发送webhook给对应的服务器
* 在我们自己的主机上开发一个简单的server，监听Datadog的webhook
* 当收到webhook后，判断是哪个监控指标导致的
* 然后通过阿里云SMS的HTTP接口调用发送短信接口
* 阿里云的SMS平台发送短信通知对应的手机！

大概的流程就是这样的，关于dquant、Datadog、阿里云的资料我最近也分别整理了如下：

* [20171210~2017mmdd-dquant](https://github.com/HackerLaboratory/_Hack/tree/master/15-open-source/20171210~2017mmdd-dquant)
* [20171210-datadog-monitor](https://github.com/HackerLaboratory/_Hack/tree/master/07-build-my-system/20171210-datadog-monitor)
* [20171213-aliyun-sms](https://github.com/HackerLaboratory/_Hack/tree/master/07-build-my-system/20171213-aliyun-sms)

下面继续总结在开发过程中遇到的一些问题、积累的经验，为后续做更多的项目开发做全面准备和积累

## 阿里云SDK API兼容Python2和Python3

参考阿里云平台关于Python SDK API的说明，开发SMS程序，但是发现阿里云的SDK只支持Python2，不支持Python3，使用Python3进行安装`python3 setup.py install`的时候可能会报错如下

```
  File "build/bdist.macosx-10.12-x86_64/egg/aliyunsdkcore/client.py", line 268
    except ValueError, TypeError:
                     ^
SyntaxError: invalid syntax


  File "build/bdist.macosx-10.12-x86_64/egg/aliyunsdkcore/http/format_type.py", line 49
    print map_format_to_accept(XML)
                             ^
SyntaxError: invalid syntax
```

针对上面两个报错，可以自己去对应修改代码为以下形式以兼容Python3

```
  except (ValueError, TypeError):

  print(map_format_to_accept(XML))
```

然后`python3 setup.py install`安装不再报错，但是后面编写出来的程序运行还是有问题

最终的解决方案是不使用具体某种开发语言的接口，而是使用SMS平台的HTTP接口，这样就不依赖于语言，在[20171213-aliyun-sms](https://github.com/HackerLaboratory/_Hack/tree/master/07-build-my-system/20171213-aliyun-sms)中对应有整理开发的方法

## WSGI的Python2和Python3的兼容性

开发下面这样的程序，在Python2下面是可以运行的

```
def application(environ, start_response):
   try:
       # the environment variable CONTENT_LENGTH may be empty or missing
       try:
          request_body_size = int(environ.get('CONTENT_LENGTH', 0))
       except (ValueError):
          request_body_size = 0

       request_body = environ['wsgi.input'].read(request_body_size)
       d = parse_qs(request_body)

       print(d)
       print(request_body)
       
       response_body = 'Datadog Webhook Response'
       status = '200 OK'
       response_headers = [('Content-Type', 'text/html'), ('Content-Length', str(len(response_body)))]
       start_response(status, response_headers)
        
       return [response_body]   

   except Exception as e:
       pass
```

但是在Python3下运行有问题，会报错，因为Python下要求最后应答的response_body不能是string，必须是bytes类型，所以可以简单进行修改

```
...
       # 必须在return之前加上这层类型转换
       response_body = bytes(response_body, encoding = "utf8")
       return [response_body]
...
```

目前我们监听Webhook的服务器就是先使用Python的wsgi简单开发的，功能上是没有问题，但是总感觉是不是当数据量大了之后还能否应对过来

这个点可以研究是不是有什么更好的解决方案！

## 性能、稳定性、易维护

最开始的实现可能也只是保证功能：能收到webhook、能发送出去短信。但是程序的稳定性、性能确实没有考虑，这些都是后续需要优化的方向，目前想到的问题大概有以下：

* 当收到一个未注册的webhook，怎么处理
* 当使用requests给阿里云发请求失败怎么办
  * 现在发送请求都是同步的，当webhook很多的时候，可能向阿里云request的请求会很多
  * 阻塞同步的话，估计程序都处理不过来
  * 想想这部分如何异步化，将服务器的性能提升起来
  * 这个技术点很不错，值得深入研究和总结
  * 在功能都实现之后，必须考虑这些问题了！！！！
* webhook、监控异常、和阿里云通信等的行为应该要落地到日志，方便后续问题排查
* 使用wsgi开发的simple server会不会性能太差，当webhook量很大的时候会不会有影响
* 除了wsgi还有没有什么更好的、成熟的、工业化的解决方案
* 如果这个服务器本身出现崩溃了要怎么办

除了上面说的性能和稳定性问题之外，还有就是代码的编写规范，需要考虑到类、封装、事件驱动引擎等的各方面，保证代码后续的易阅读、易维护、易修改，比如

* aliyun平台是不是可以封装一下，或者以什么样的方式进行封装
* datadog平台是不是可以封装一下
* webhook建议以事件驱动引擎的方式管理，每增加一个webhook对应注册一个处理方法即可

## 开发平台

在这个小模块开发的过程中，还有一个很重要的收获，就是熟悉了Datadog、阿里云这些平台的使用，以前自己想要做什么项目总是想所有的东西都自己来搞，其实很多云平台提供了不错的服务，完全可以有效利用这些资源

* Datadog的监控功能（Datadog的功能真的很强大！）
* 使用阿里云平台出现问题的话，可以在【控制台-->工单】上向阿里云的工程师提问，他们响应还是比较快的
* 阿里云的SMS功能
* 阿里云的服务器（MySQL、Redis、MongoDB……）服务
* 其他平台

想要实现自己的一个想法，现在有诸多的可用的资源、开源组件等来帮助自己搭建系统，不要太死脑筋！

我一直说技术有两个方向：

* 技术实践：利用像redis、mysql、MongoDB、nginx、docker、等技术实现自己的一些创意想法
* 技术原理：网络编程、编译原理、算法与数据结构等原理领域的深入研究

接下来可以先主要投入时间在【技术实践】方面，积累实践、功能实现编码、调优编码、架构编码、运维等方面的经验，后续在更多的投入到【技术原理】方面

## 简单总结

这篇文章是在自己开发过程中简单输出的一份文档，并不是项目完成后的总结，所以很好的记录了开发过程中的心路、思考、大坑小坑等

虽然对很多问题都没有给出最终解决方案，都只是简单的列举出来，但是真的更具有参考价值和反思价值

当然上面提到的诸多问题后续会自己研究，并且给出最终解决方案，并整理成文，包括：

* 如何将现在的监控服务器异步化，保证能够处理大量的webhook，及时发短信通知
  * 最重要的是，异步模式实现后，必须要对同步和异步模式分别进行性能测试
  * 只有拿到最终的性能测试数据才是最好的作为方案选择的依据！
* wsgi是不是有什么更成熟、工业化的替代方案
  * 顺便将wsgi的原理进一步研究一下！

最终在这个项目中开发使用到的技术、钻研的技术、遇到的问题等都有必要输出文档，作为自己技术成长的见证！
