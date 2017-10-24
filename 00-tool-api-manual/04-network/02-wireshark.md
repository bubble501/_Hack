WireShark可以这样设置过滤IP、端口

```
((ip.src==192.168.39.28) or (ip.dst==192.168.39.28)) and ((ip.dst==192.168.163.41 and tcp.dstport==1433) or (ip.src==192.168.163.41 and tcp.srcport==1433))
```

![image](./image/02-01.png)
