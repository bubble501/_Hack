## LT和ET模式

epoll对文件描述符的操作有两种模式：LT(Level Trigger，电平触发)模式和ET(Edge Trigger，边沿触发)模式

LT模式是默认的工作模式，这种模式下epoll相当于一个效率较高的poll

当往epoll内核事件表中注册一个文件描述符上的EPOLLET事件时，epoll将以ET模式来操作该文件描述符，ET模式是epoll的高效工作模式

对于采用LT工作模式的文件描述符，当epoll_wait检测到其上有时间发生并将此时间通知应用程序后，应用程序可以不立即处理该事件。这样，当应用程序下一次调用epoll_wait时，epoll_wait还会再向应用程序通告此时间，直到该事件被处理

而对于采用ET工作模式的文件描述符，当epoll_wait检测到其上有事件发生并将此事件通知应用程序后，应用程序必须立即处理该事件，因为后续的epoll_wait调用将不会再向应用程序通知这一事件

可见ET模式在很大程度上降低了同一个epoll事件被重复触发的次数，因此效率要比LT模式高

本次练习展示LT和ET在工作模式上的差异

## 运行效果

终端`./epoll_lt_et 127.0.0.1 12346`，浏览器输入`127.0.0.1:12346`进行访问

LT模式下终端输出信息如下

```
event trigger once
get 9 bytes of content: GET / HTT
event trigger once
get 9 bytes of content: P/1.1
Ho
event trigger once
get 9 bytes of content: st: 127.0
event trigger once
get 9 bytes of content: .0.1:1234
event trigger once
get 9 bytes of content: 6
Connec
event trigger once
get 9 bytes of content: tion: kee
event trigger once
get 9 bytes of content: p-alive

event trigger once
get 9 bytes of content: Upgrade-I
event trigger once
get 9 bytes of content: nsecure-R
event trigger once
get 9 bytes of content: equests: 
event trigger once
get 9 bytes of content: 1
User-A
event trigger once
get 9 bytes of content: gent: Moz
event trigger once
get 9 bytes of content: illa/5.0 
event trigger once
get 9 bytes of content: (X11; Lin
event trigger once
get 9 bytes of content: ux x86_64
event trigger once
get 9 bytes of content: ) AppleWe
event trigger once
get 9 bytes of content: bKit/537.
event trigger once
get 9 bytes of content: 36 (KHTML
event trigger once
get 9 bytes of content: , like Ge
event trigger once
get 9 bytes of content: cko) Ubun
event trigger once
get 9 bytes of content: tu Chromi
event trigger once
get 9 bytes of content: um/51.0.2
event trigger once
get 9 bytes of content: 704.79 Ch
event trigger once
get 9 bytes of content: rome/51.0
event trigger once
get 9 bytes of content: .2704.79 
event trigger once
get 9 bytes of content: Safari/53
event trigger once
get 9 bytes of content: 7.36
Acc
event trigger once
get 9 bytes of content: ept: text
event trigger once
get 9 bytes of content: /html,app
event trigger once
get 9 bytes of content: lication/
event trigger once
get 9 bytes of content: xhtml+xml
event trigger once
get 9 bytes of content: ,applicat
event trigger once
get 9 bytes of content: ion/xml;q
event trigger once
get 9 bytes of content: =0.9,imag
event trigger once
get 9 bytes of content: e/webp,*/
event trigger once
get 9 bytes of content: *;q=0.8

event trigger once
get 9 bytes of content: Accept-En
event trigger once
get 9 bytes of content: coding: g
event trigger once
get 9 bytes of content: zip, defl
event trigger once
get 9 bytes of content: ate, sdch
event trigger once
get 9 bytes of content: 
Accept-
event trigger once
get 9 bytes of content: Language:
event trigger once
get 9 bytes of content:  en-US,en
event trigger once
get 9 bytes of content: ;q=0.8

event trigger once
get 1 bytes of content: 


```

ET模式下终端输出信息如下

```
event trigger once
get 9 bytes of content: GET / HTT
get 9 bytes of content: P/1.1
Ho
get 9 bytes of content: st: 127.0
get 9 bytes of content: .0.1:1234
get 9 bytes of content: 5
Connec
get 9 bytes of content: tion: kee
get 9 bytes of content: p-alive

get 9 bytes of content: Upgrade-I
get 9 bytes of content: nsecure-R
get 9 bytes of content: equests: 
get 9 bytes of content: 1
User-A
get 9 bytes of content: gent: Moz
get 9 bytes of content: illa/5.0 
get 9 bytes of content: (X11; Lin
get 9 bytes of content: ux x86_64
get 9 bytes of content: ) AppleWe
get 9 bytes of content: bKit/537.
get 9 bytes of content: 36 (KHTML
get 9 bytes of content: , like Ge
get 9 bytes of content: cko) Ubun
get 9 bytes of content: tu Chromi
get 9 bytes of content: um/51.0.2
get 9 bytes of content: 704.79 Ch
get 9 bytes of content: rome/51.0
get 9 bytes of content: .2704.79 
get 9 bytes of content: Safari/53
get 9 bytes of content: 7.36
Acc
get 9 bytes of content: ept: text
get 9 bytes of content: /html,app
get 9 bytes of content: lication/
get 9 bytes of content: xhtml+xml
get 9 bytes of content: ,applicat
get 9 bytes of content: ion/xml;q
get 9 bytes of content: =0.9,imag
get 9 bytes of content: e/webp,*/
get 9 bytes of content: *;q=0.8

get 9 bytes of content: Accept-En
get 9 bytes of content: coding: g
get 9 bytes of content: zip, defl
get 9 bytes of content: ate, sdch
get 9 bytes of content: 
Accept-
get 9 bytes of content: Language:
get 9 bytes of content:  en-US,en
get 9 bytes of content: ;q=0.8

get 1 bytes of content: 

read later

```
