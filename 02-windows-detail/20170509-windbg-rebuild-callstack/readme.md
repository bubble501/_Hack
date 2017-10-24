>转载自[《怎样重建一个损坏的调用堆栈（callstack）》](http://blog.csdn.net/magictong/article/details/28161757)

---

在我的日常工作中，我经常阅读来自微软的[WinQual](https://en.wikipedia.org/wiki/Winqual)的报告。这些报告里面一般包含着dump文件，从这些dump文件里面我可以分析一些常见的软件里面到底出了什么问题，造成它崩溃了。总而言之，这是一个超赞的系统，我强烈建议各个独立软件开发商去上面注册（尤其是这个系统对任何人都是免费的，只要你的可执行文件是正确签名的）。最近我拿到了一个堆栈已经严重破坏的dump文件，我想和大家讨论一下怎么使用WinDbg工具来重建它的调用堆栈（caall stack）

在开始之前，让我们先看看一个原始的调用堆栈是什么样子的，在WinDbg里面运行`k`命令即可

```
0:000> k
ChildEBP RetAddr  
028b89cc 77c75350 ntdll!KiFastSystemCallRet
028b89d0 77c4b208 ntdll!ZwTerminateProcess+0xc
028b89e0 763e41ec ntdll!RtlExitUserProcess+0x7a
028b89f4 10056386 kernel32!ExitProcess+0x12
WARNING: Stack unwind information not available. Following frames may be wrong.
028b89fc 100565a0 EyeOneIO!I1_SynchronizeWhitebases+0xf0f6
028b8a0c 10054803 EyeOneIO!I1_SynchronizeWhitebases+0xf310
00000000 00000000 EyeOneIO!I1_SynchronizeWhitebases+0xd573
```

从上面的调用堆栈来看，有几个特征表明这个堆栈已经被破坏了。首先，调用堆栈的基址不可能从0x00000000开始。通常情况下，它从main函数的入口地址开始，或者从一个线程的入口地址开始，但从上面的调用堆栈来看我们没有看到这个特征。另外，WinDbg也发出了“Stack unwind information notavailable. Following frames may be wrong”的警告

**第一步**

既然堆栈已经错误了，我们当然需要重建当前执行线程的堆栈，并找到当前线程堆栈的起始位置。这里有个简单的扩展名来可以查看，使用`!teb`即可（`!teb`用于查看当前线程的执行环境）：

```
0:000> !teb
TEB at 7ffdb000
    ExceptionList:        028b8a28
    StackBase:            028c0000
    StackLimit:           028b6000
    SubSystemTib:         00000000
    FiberData:            00001e00
    ArbitraryUserPointer: 00000000
    Self:                 7ffdb000
    EnvironmentPointer:   00000000
    ClientId:             00000a4c . 00000e3c
    RpcHandle:            00000000
    Tls Storage:          7ffdb02c
    PEB Address:          7ffdf000
    LastErrorValue:       14007
    LastStatusValue:      c0150008
    Count Owned Locks:    0
    HardErrorMode:        0
```

**第二步**

看上面`!teb`命令显示的结果里面，StackBase和StackLimit告诉了我们当前线程的堆栈在内存中的范围，因此我们可以转储这个范围的地址，然后从里面寻找一些有意义的和有用的东西（就是将内存地址和对应的符号地址对应起来，然后寻找和当前的线程有关的调用堆栈）

WinDbg里面有个专门的`dds`命令就是用来做这个事情的，`dds`命令需要你指定一个起始的地址，然后它从给定的起始地址开始转储一定范围的地址，并且尝试把每个地址里面的内容和符号（Symbol）对应起来（假如可以对应的话）。`dds`转储的内容包含三列数据，第一列显示的是顺序递增的地址，第二列显示的是地址里面的内容，第三列是符号名称（如果地址里面的数据可以被成功的解析为一个符号的话，否则第三列就是显示的空白）

把真是的栈转储出来看看，使用`dds 028b6000 028c0000`来转储028b600到028c0000之间的内存

```
028b6000  00000000
...
028bf9d8  00000000
028bf9dc  00000000
028bf9e0  79035b7f
028bf9e4  028bfa1c
028bf9e8  6e760b5b i1IO!i1IO::measureOneStrip+0xbb
028bf9ec  42b840fc
...
028bfa18  00000000
028bfa1c  028bfd98
028bfa20  6e763387 i1IO!i1IO::_measureSingleRowScanThreaded+0x1467
028bfa24  42b840fc
...
028bfd94  00000006
028bfd98  028bfe2c
028bfd9c  6e761062 i1IO!i1IO::_advancedMeasureThreaded+0x222
028bfda0  013a8520
028bfda4  79035e2e
...
028bfe28  00000000
028bfe2c  028bfe38
028bfe30  763ed0e9 kernel32!BaseThreadInitThunk+0xe
028bfe34  012118e0
028bfe38  028bfe78
028bfe3c  77c516c3 ntdll!__RtlUserThreadStart+0x23
028bfe40  012118e0
...
028bfe74  00000000
028bfe78  028bfe90
028bfe7c  77c51696 ntdll!_RtlUserThreadStart+0x1b
028bfe80  6e760e40 i1IO!i1IO::_advancedMeasureThreaded
...
028c0000  ????????
```

实际上转储出来的堆栈比上面列出来的要大得多，不过为了简单起见，我只保留一些相关的部分

**第三步**

现在要做的第一件事就是定位到callstack的起始位置。在这个例子里面，RtlUserThreadStart看起来很像是这个起始位置，因为它是线程的起始调用函数。在找到这个起始点之后，获取起始点的前一个堆栈地址A（第一列），然后在兑现的内容里面（第二列）寻找是否有等于A的堆栈B（**向低地址寻找，因为堆栈是向低地址增长的**），然后再在堆栈内容里面寻找是否有等于B的堆栈地址C......，按照这种方法不断的搜索内存，直到不能再找到任何东西或者找到空地址

>这个就是利用的标准函数堆栈的基本原理，对此处不理解的可以去了解一下标准函数栈帧，一般没有经过FPO优化的调用函数链，可以通过EBP的值在整个堆栈上面串联起来，其实WinDbg自己也是这么找的，而本文讨论的恰恰是因为堆栈被破坏之后，WinDbg找不到正确的callstack之后，我们怎么手动恢复的问题

在这个例子中，我们先从下面的堆栈开始找：

```
028bfe78  028bfe90
028bfe7c  77c51696 ntdll!_RtlUserThreadStart+0x1b
```

搜索地址028bfe78，找到下面的堆栈：

```
028bfe38  028bfe78
028bfe3c  77c516c3 ntdll!__RtlUserThreadStart+0x23
```

搜索地址028bfe38，得到下面的堆栈：

```
028bfe2c  028bfe38
028bfe30  763ed0e9 kernel32!BaseThreadInitThunk+0xe
```

搜索地址028bfe2c，得到下面的堆栈：

```
028bfd98  028bfe2c
028bfd9c  6e761062 i1IO!i1IO::_advancedMeasureThreaded+0x222
```

搜索地址028bfd98，得到下面的堆栈：

```
028bfa1c  028bfd98
028bfa20  6e763387 i1IO!i1IO::_measureSingleRowScanThreaded+0x1467
```

搜索地址028bfa1c，得到下面的堆栈：

```
028bf9e4  028bfa1c
028bf9e8  6e760b5b i1IO!i1IO::measureOneStrip+0xbb
```

**第四步**

现在，继续搜索028bf9e4已经不能再在堆栈里面找到信息了，也就是说我们可能已经找到了最终出问题的函数位置，我们可以使用WinDbg尝试修复我们的callstack，当然我们需要给它们上面找到的这些信息，其实很简单，只要上面没有找错，我们给k命令指明一个明确的地址，通过L参数传递进去（用上面我们最后找到的028bf9e4），那么WinDBg马上就会给我们一个更加友好的callstack信息

```
0:000> k L=028bf9e4
ChildEBP RetAddr  
028b89cc 77c75350 ntdll!KiFastSystemCallRet
028b89d0 77c4b208 ntdll!ZwTerminateProcess+0xc
028bf9e4 6e760b5b ntdll!RtlExitUserProcess+0x7a
028bfa1c 6e763387 i1IO!i1IO::measureOneStrip+0xbb
028bfd98 6e761062 i1IO!i1IO::_measureSingleRowScanThreaded+0x1467
028bfe2c 763ed0e9 i1IO!i1IO::_advancedMeasureThreaded+0x222
028bfe38 77c516c3 kernel32!BaseThreadInitThunk+0xe
028bfe78 77c51696 ntdll!__RtlUserThreadStart+0x23
028bfe90 00000000 ntdll!_RtlUserThreadStart+0x1b
```

现在我们看到的callstack是不是更加完成并且合理了？！没有了调用栈帧错误的警告，而且callstack的调用地址也正常了

>希望上面介绍的这种方法能给你的调试工作带来一些帮助

