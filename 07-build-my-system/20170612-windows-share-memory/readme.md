>共享内存可以实现跨进程的数据传输，如果再配合事件对象通知两个进程（具体为两个进程中的不同线程）何时去读、去写，就可以实现一个同机器下多进程通信的逻辑。下面先讨论一下协议设计、性能优化的部分，针对使用事件对象、共享内存实现通信的逻辑模式会在后面总结

## 通信协议的设计

虽然是走共享内存，但和网络通信除了通道不同之外其实都是想通的，更具体一点就是和阻塞IO编程是一致的，所以在这里如何设计通信的协议是极其重要的

初步给出一个最简单的设计：

* 请求：请求ID、请求内容长度、请求内容
* 应答：对应的请求ID（用于应答回来判断请求应答是否一一对应）、应答内容长度、应答内容

其中请求和应答内容不使用字符串格式，而直接传递二进制的结构体内容

## 性能优化

使用共享内存实现通信、数据传输，有不同的需求场景，比如我们进程化框架上行纯粹就是为了实现主子进程之间的控制；但下行应用这种模式是为了快速传输大量的数据，所以这其中的性能要求就很高

所以去梳理一下流程，然后结合测试找到瓶颈点，然后针对性的优化：

* 子进程子线程往共享内存中写入请求
	* 先在自己内存中按请求格式组织数据
	* 然后将数据拷贝到共享内存中
* 子进程子线程SetEvent通知主进程子线程
* 主进程子线程获取请求
	* 拷贝共享内存中的请求到自己的内存
	* 按照请求格式解析请求数据
* 主进程子线程处理请求
	* 处理，获取应答数据
* 主进程子线程往共享内存中写入应答
	* 先在自己内存中按应答格式组织数据
	* 然后将数据拷贝到共享内存中
* 主进程子线程SetEvent通知子进程子线程
* 子进程子线程获取应答
	* 拷贝共享内存中的应答到自己的内存
	* 按照应答格式解析应答数据

简单的过一遍上面的流程，发现主要的操作在：

* 事件对象的操作
* 打包、解包请求和应答数据
* 数据在内存中的拷贝，隐含着内存的申请和释放
* 另外可能还有为了保证线程安全的临界区操作

简单分析一下，哪些是必不可少的，哪些是可以优化的：

* 事件对象的操作，因为这是通信的关键，所以必不可少
* 打包、解包请求和应答数据
	* 像FIX、HTTP等协议的实现中按照一定格式打包解包都是必不可少的
	* 但是如果能定义一些标准的结构体，直接在内存中传输二进制，那么所有打包解包的逻辑都省了！
* 进程内存和共享内存间的数据拷贝：是通信的关键，所以必不可少
* 内存的申请和释放
	* 如果能确定内存的大小，那么一次性申请，然后持续使用就可以省去频繁的内存申请释放
	* 而且稳定的内存使用对于内存碎片等问题也是很好的解决方案
	* 函数参数传指针而不是结构体，可以有效避免栈上的拷贝！
	* 比如二进制网络协议、传输二进制结构体，尽量通过强转而不是内存拷贝来得到对应的结构体
* 另外逻辑、判断、循环等这些东西能少则少！尽可能提升性能！
* 尽量减少循环
* 尽量减少不必要的逻辑判断
* 磁盘操作尽量减少
* 尽量减少加锁同步，其实现在下行共享内存实现中有大量的加锁同步
* 如果有数据库，注意索引设计、SQL编写逻辑，提升数据库性能

## 多线程同步技巧

主进程内部好实现，针对每个不同的子进程分别创建一个专门的线程来处理应答，所以没有并发安全的问题；但是子进程内部就复杂了，同样的接口可能会有多个线程去同时调用，那么就可能出现多个线程同时操作事件对象、共享内存的可能，这样是非线程安全的，必须进行同步控制！

demo1使用队列存储请求，所有线程的请求放到一个队列中，然后有一个专门的线程从队列中取出请求来向主进程发起请求。但是这样对于及时唤醒处理请求线程、保证请求和应答的一一对应很难控制

demo2在每个函数内部使用临界区锁来进行同步，这样的实现方式简单的多，之前的很多同步问题都解决了，这样逻辑上更简单些。大概性能如下：

* 一个进程中2个线程同时调用GetStockInfo，每个线程调用10000次
	* 每个线程耗时1s左右，两个线程一起相当于20000笔/s
* 一个进程中1个线程同时调用GetStockInfo，每个线程调用10000次
	* 调用10000次耗时400ms，相当于25000笔/s
	* 因为有加锁同步，所以多线程反而更慢些

demo3直接不需要另外一个中间线程，每个线程直接和子进程交互，通过临界区同步子进程内部多线程，通过事件对象控制主子进程通信。大概性能情况如下：

* 一个进程中2个线程同时调用GetStockInfo，每个线程调用10000次
	* 每个线程耗时400s左右，两个线程一起相当于50000笔/s
* 一个进程中1个线程同时调用GetStockInfo，每个线程调用10000次
	* 调用10000次耗时220ms，相当于40000笔/s
* 既减少了一个线程，而且性能大概提升到50000笔/s

>使用临界区来同步线程，将多个线程的并行给串行化，这个在之前的开发中是应用的很广的了，但是这次做共享内存开发，涉及到多进程、多线程、事件对象等这些复杂的东西，结果把这么简单方便的线程同步方法给忘了，导致demo1、demo2走了不少弯路。当然使用锁来同步线程可能会导致性能下降，但经过测试在可接受的范围内

## 性能测试

最终使用上面的内存拷贝机制、锁同步机制，可以达到不低于20000条记录/s的性能

## 正确的交互逻辑

>正常情况下可以保证请求和应答的一一对应

**在创建的地方**

```
begin
	hDealerHd1 := CreateEvent(nil, True, True, 'testDeal1');
	hDealerHd2 := CreateEvent(nil, True, True, 'testDeal2');
	ResetEvent(hDealerHd1);
	ResetEvent(hDealerHd2);
end;
```

**请求的循环中**

```
begin
	hCallerHd1 := OpenEvent(EVENT_ALL_ACCESS, True, 'testDeal1');
    hCallerHd2 := OpenEvent(EVENT_ALL_ACCESS, True, 'testDeal2');

    for i:= 1 to 5000 do
    begin
    	//往共享内存写请求
		SetEvent(hCallerHd1);
		iRet := WaitForSingleObject(hCallerHd2, WaitTime);
		if iRet = WAIT_OBJECT_0 then
		begin
			ReSetEvent(hCallerHd2);
			//从共享内存读应答
		end;
		if WAIT_TIMEOUT = iRet then
		begin
			Log.AddLog(0, '第%d次请求超时', [i], 'request');
		end;
    end;
end;
```

**应答的循环中**

```
begin
	while not Terminated do
	begin
		//等500，如果对方长时间没有SetEvent，那么这里就会超时继续执行
		//因为ResetEvent(FDealerHd1);在判断里面，所以可以保证请求应答的一致
		if WaitForSingleObject(FDealerHd1, 500) = WAIT_OBJECT_0 then
		begin
			ResetEvent(FDealerHd1);
			try
				//从共享内存读请求
				//处理请求
				//写应答到共享内存
			finally
				SetEvent(FDealerHd2);
			end;
		end;
	end;
end;
```

## 会出问题的交互逻辑

>可能导致请求和应答错位，出现大问题

**创建的地方**

```
begin
	hDealerHd1 := CreateEvent(nil, True, True, 'testDeal1');
    hDealerHd2 := CreateEvent(nil, True, True, 'testDeal2');
end;
```

**请求的循环中**

```
begin
	hCallerHd1 := OpenEvent(EVENT_ALL_ACCESS, True, 'testDeal1');
    hCallerHd2 := OpenEvent(EVENT_ALL_ACCESS, True, 'testDeal2');

    for i:= 1 to 5000 do
    begin
		//往共享内存写请求
		ReSetEvent(hCallerHd2);		//可能这里面先Reset，还没有走到WaitFor的地方，对方就已经Set了，所以这样可能导致问题
		SetEvent(hCallerHd1);
		iRet := WaitForSingleObject(hCallerHd2, WaitTime);
		if iRet = WAIT_OBJECT_0 then
		begin
			//从共享内存读应答
		end;
		if WAIT_TIMEOUT = iRet then
		begin
			Log.AddLog(0, '第%d次请求超时', [i], 'request');
		end;
	end;
end;
```

**应答的循环中**

```
begin
	while not Terminated do
	begin
		ResetEvent(FDealerHd1);
		if WaitForSingleObject(FDealerHd1, 500) = WAIT_OBJECT_0 then
		begin
			try
				//从共享内存读请求
				//处理请求
				//往共享内存写应答
			finally
				SetEvent(FDealerHd2);
			end;
		end;
	end;
end;
```
