>[《Web开发者安全速查表》](http://www.freebuf.com/articles/web/135278.html)

>想要开发出一个安全的、健壮的Web应用其实是非常困难的，如果你觉得这实现起来简单的话，那么要不你就是吊炸天的程序员，要不你就是在做白日梦

## 写在前面的话

如果你觉得你可以在一个月之内开发出一款集使用价值、用户体验度、以及安全行为一身的产品，那么在你将产品原型真正推上市场之前，请一定要三思啊！

当你仔细审查了本文给出的安全小贴士之后，你可能会发现你在产品的开发阶段跳过了很多重要的安全步骤。有时候，也许你应该对的用户坦诚一些，你应该诚实的告诉他们这款产品还没有完全搞定，还有很多的安全问题亟待解决

下面这份速查表非常简洁，而且绝对还有很多东西没有涉及到。就我个人而言，我从事安全Web应用开发工作已经超过14年了，而本文给出的小贴士都是让我在过去一段时间里曾痛苦不堪的重要安全问题。我希望大家可以认真对待，不仅是对用户负责，也要对自己的职业生涯负责

## 数据库篇

1. 对类似访问令牌、电子邮箱地址或账单详情进行加密处理，尤其是用户的身份识别信息（密码）
2. 如果你的数据库支持低成本加密，请确保开启这项功能并保护主机磁盘中的数据。于此同时，确保所有的备份文件都进行了加密存储
3. 按照最小权限原则给数据库访问账号分配权限，不要使用数据库的root账户
4. 使用秘钥存储器来保存和派发秘钥，不要直接将秘钥硬编码在你的应用之中
5. 通过使用SQL预处理语句来避免SQL注入攻击。比如说，如果你使用的是NPM，那么请不要使用npm-mysql，你应该用的是npm-mysql2，因为它支持SQL预处理语句

## 开发篇

1. 确保你软件中所有组件的每一个版本都进行了漏洞扫描，包括接口、协议、代码以及数据包
2. 对产品中所有使用到的第三方工具时刻保持警惕性，选择一款安全系数较高的开发平台

## 身份验证篇

1. 使用合适的加密算法（例如bcrypt）来计算并存储密码哈希，在初始加密时选择合适的随机数据，还有就是千万不要自己去写一个加密算法
2. 使用简单但健壮的密码规则，以鼓励用户设置长度足够安全的随机密码
3. 在服务的登录机制中引入多因素身份验证功能

## Dos保护篇

1. 确保那些针对API的Dos攻击不会严重影响你网站的正常运行，至少要限制API的请求访问速率
2. 对所有用户所提交的数据和请求进行结构和大小的限制
3. 使用类似[CloudFlare](https://www.cloudflare.com/)这样的缓存代理服务来为你的Web应用添加[DDos缓存方案](https://en.wikipedia.org/wiki/Denial-of-service_attack)

## Web流量篇

1. 使用TLS，不只是你的登录表单和网站响应数据，而是你的整个网站都应该使用TLS
2. Cookie必须使用httpOnly
3. 使用CSP（内容安全策略），虽然配置过程比较麻烦，但这绝对是值得的
4. 在客户端响应中使用X-Frame-Option和X-XSS-Protection头
5. 使用HSTS响应，使用HTTPS
6. 在所有的表单中使用CSRF令牌

## API篇

1. 确保你所有的公共API中没有可以枚举的资源
2. 确保用户在使用你的API之前，对他们的身份进行验证

## 验证篇

1. 在客户端对用户的输入进行验证，并即使给予反馈（AJAX），但永远不要相信用户输入的数据
2. 在服务器端再对用户所输入的每一个字符进行一次彻底的验证，永远不要直接将用户输入的内容注入到响应数据中，永远不要直接在SQL语句中插入用户提供的数据

## 云端配置篇

1. 确保所有的端口只开启必要的端口，关闭不用的端口，并对常用端口进行强制性的安全保护，因为通过非标准端口来进行攻击对于攻击者而言相对来说比较困难
2. 确保服务器后台数据库和后台服务无法通过公网查看到
3. 在单独的VPC节点配置逻辑服务或提供服务内通信
4. 确保所有的服务只接受来自有限IP地址的数据
5. 限制输出数据的IP地址以及端口
6. 使用AWS IAM角色。，不要使用root凭证
7. 对所有的管理员和开发人员提供最小的访问权限
8. 定期更换密码和访问秘钥

## 基础设置篇

1. 确保可以在主机不下线的情况下进行更新操作，确保部署了全自动化的软件更新策略
2. 使用类似Terraform这样的工具来创建所有的基础设置，不要使用云端console（控制台）来进行创建
3. 对所有服务的日志进行集中记录，不要通过SSHH来访问或获取日志
4. 不要让AWS服务组的端口22保持开启状态
5. 一定要部署入侵检测系统

## 操作篇

1. 关闭不用的服务和服务器，因为最安全的服务器就是那些关闭着的服务器

## 测试篇

1. 开发完成后，对你的设计和代码实现进行多次安全审查
2. 进行渗透测试，也就是自己黑自己，但你也要让别人来对你的网站进行渗透测试

## 计划篇

1. 创建一个安全威胁模型，用来描述你可能会遇到的威胁以及攻击者
2. 设计一个安全应急响应方案，你总有一天会用到的

## 自我反思

自己目前在做设计和开发的时候完全没有考虑过这方面的问题

主要的问题是自己对于Web攻击的各种方法都不清楚，所以具体该怎么防范这些攻击也没有什么好的方案。也许真的要在自己平时做开发之余对于安全攻防的进行大量的实践和学习，主要是安全的知识涉及面很广，可以学到更全面更深刻的计算机知识：

* 网络原理
* 加密解密
* 数据库
* 软件调试
* 测试
* 维护