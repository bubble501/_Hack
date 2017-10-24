## 说明

参考[《Python调用C语言函数》](http://coolshell.cn/articles/671.html)

使用Python的ctypes，我们可以直接调用由C直接编译出来的函数，其实就是调用动态链接库中的函数

为什么我们需要这样做呢？因为有些时候，我们可能需要一个性能上比较讲究的算法；有些时候，我们可以在Python中使用已经有的现成的被封装在动态链接库中的函数

## 下面是如何调用的示例

首先，用C实现一个乘法的功能

```
int 
multiply(int num1, int num2)
{
    return num1 * num2;
}
```

如果实在Windows下，你可能需要写成这样

```
#include <windows.h>

BOOL APIENTRY 
DllMain(HANDLE hModule, DWORD dwReason, LPVOID lpReserved)
{
    return TRUE;
}

__declspec(dllexport) int
multiply(int num1, int num2)
{
    return num1 * num2;
}
```

在Linux下将这个C文件编译成动态链接库

```
$ gcc -c -fPIC libtest.c		#生成libtest.o
$ gcc -share libtest.o -o libtest.so	#生成libtest.so
```

如果是在Windows下面，则这样编译

```
cl -LD libtest.c -libtest.dll
```

接下来可以在Python中实现这样的程序，如果实在Windows下面，则把libtest.so改成libtest.dll即可

```
# -*- coding: utf-8 -*-
from ctypes import *
import os

if __name__ == '__main__':
    libtest = cdll.LoadLibrary(os.getcwd() + '/libtest.so')
    print libtest.multiply(2, 3) 
```

执行`python testPythonC.py`即可看到执行效果

## 补充说明

目前在Windows下开发，经常会有这样的场景：VC++实现一些基础的功能，封装好之后，通过DLL的方式提供外界可调用的API，然后Delphi开发的时候可以加载VC++开发的DLL，调用DLL中的导出函数

其实这里面展示的Python配合C开发也基本是一样的道理：Linux上导出so动态链接库、Windows上导出dll动态链接库，然后Pyhon可以加载C开发的动态链接库，然后调用动态链接库中导出的方法

实际的运用中远不止于做个乘法这么简单，完全可以充分利用这个特点，正如上面所提到的

* 有些时候，我们可能需要一个性能上比较讲究的算法
* 有些时候，我们可以在Python中使用已经有的现成的被封装在动态链接库中的函数
