>[《Python量化交易平台开发教程系列3-vn.py项目中API封装的编译》](http://www.vnpy.org/basic-tutorial-3.html)

# 前言

经历了两篇的理论折磨后，本篇教程开始进入实际操作的环节，这里作者假设读者是毫无C++经验的用户，操作一步步配图，还有问题的来[vn.py项目的github主页](https://github.com/vnpy/vnpy)上提问

本篇将会包含的内容：

* 安装Anaconda （一次安装搞定95%以上量化相关包的Python发行版）
* 安装Visual Studio
* 编译Boost库
* 编译vn.lts

# 安装Anaconda

在[Anaconda历史版本](http://repo.continuum.io/archive/index.html)中下载1.9.2（win 32位版本）

![image](./image/03-01.jpg)

下载好后双击安装，流程没有特别需要注意的，安装文件夹作者选的是D:\Anaconda，读者可以自己选择，注意最后一步编译vn.lts中设置时的文件夹必须是这里的安装文件夹

为什么不用最新版本：尝试过使用2.1.0版，发现有一些包在我的电脑上存在兼容性问题（可能因为中文支持不好），需要修改一些.py文件里的源代码，太过麻烦，而且1.9.2到2.1.0的区别非常小

为什么不用64位版本：同样是因为发生过兼容性的问题，不怕麻烦的读者可以自己折腾看看

# 安装Visual Studio

在[Visual Studio下载](https://www.visualstudio.com/downloads/download-visual-studio-vs)中下载Community 2013 with Update 4，注意选择简体中文（英文好的随意）

![image](./image/03-02.jpg)

下好了同样正常安装，安装文件夹随意，这个对后面编译没有影响

不想用2013的其他版本的VS应该也都没问题，作者还测试过2010

# 编译Boost库

### 下载解压缩

在[Source Forge](http://sourceforge.net/projects/boost/files/boost/1.57.0/)下载Boost 1.57.0，Source Forge下载速度较慢，建议选择7z格式

![image](./image/03-03.jpg)

下载完成后进行解压缩，将Boost库放到D:\boost_1_57_0文件夹下，注意最后一步编译vn.lts同样会用到这个文件夹

### Visual Studio命令提示

打开Visual Studio的安装目录，逐步进入.\Common7\Tools\Shortcuts，找到Developer Command Prompt for VS2013，打开

![image](./image/03-04.png)

打开后的窗口就是个cmd命令窗口，接下来我们需要在其中敲入一些命令

记得我们之前Boost库放在了D:\boost_1_57_0这个文件夹下，首先我们要切换到这个文件夹，逐行输入以下命令，输完后记得回车：

```
d:

cd\boost_1_57_0
```

此时cmd里的显示应该类似于： 

![image](./image/03-05.jpg)

### booststrap.bat

然后输入以下内容，用于生成b2.exe文件：

```
bootstrap.bat
```

成功后显示为：

![image](./image/03-06.jpg)

### b2.exe

接下来编译boost库，--toolset=msvc-12.0是因为作者使用的是VS2013, --build-type=complete编译整个boost库，因为项目中不止用到boost.python：

```
b2 --toolset=msvc-12.0 --build-type=complete
```

这一步耗时会较长，视乎你的电脑性能，作者这里耗时大约20多分钟，过程中也会出现很多奇怪的输出，忽略就好，成功后会看到：

![image](./image/03-07.jpg)

这里D:\boost_1_57_0\stage\lib中就是我们编译好的Boost库

# 编译vn.lts

>本教程中的例子是行情API（vnltsmd）

### 下载vn.py

vn.py项目托管在Github上，主页为[https://github.com/vnpy/vnpy](https://github.com/vnpy/vnpy)，点击主页右侧的“Download ZIP”下载（下图红框）

![image](./image/03-08.jpg)

解压缩zip文件，将\vnpy\api\lts\ltsapi文件夹下的所有文件（.so文件除外）以及vnpy\api\lts\vnltsmd\vnltsmd文件夹下的所有文件，复制到一个新的文件夹中（假设命名为new），内容如下图所示

![image](./image/03-09.jpg)

### 创建VS项目

打开VS2013后，点击菜单栏的“文件” -> “新建” -> “项目”， 在弹出的窗口中的左侧选择“模板” -> “Visual C++” -> “Win32” -> “Win32项目”，下方的名称填入“vnltsmd”， 位置填入“D:\”（参见下图）。

![image](./image/03-10.jpg)

点击“确定”后，弹出Win32应用程序向导，点击“下一步”，应用程序类型选择为“DLL”，点击“完成”。

### 添加文件

在左侧的解决方案管理器中，将已有的文件全部删除。然后右键点击vnltsmd，选择“添加” -> “现有项”， 将之前new文件夹中所有的文件添加进来，如下图：

![image](./image/03-11.jpg)

添加完成后的解决方案如下图：

![image](./image/03-12.jpg)

### 配置项目

点击上方工具栏中的解决方案配置选项框（默认显示应该是“Debug”），选择“Release”，如下图红色方框： 

![image](./image/03-13.jpg)

右键点击方案管理器中的vnltsmd，选择“属性”，打开“vnltsmd属性页”对话框

选择“配置属性” -> "VC++目录” ，如下图：

![image](./image/03-14.jpg)

在包含目录中添加：

* D:\boost_1_57_0
* D:\Anaconda\include
* new文件夹

在引用目录中添加：

* D:\boost_1_57_0\libs\
* D:\Anaconda\libs
* new文件夹

然后点击“链接器”，在附加库目录中添加：

* D:\boost_1_57_0\stage\lib
* D:\Anaconda\libs
* new文件夹

最后点击“C/C++” -> "预编译头”，将“预编译头”设置为“不使用预编译头”

都设置完了记得点“确定”，不然又得重弄一遍。

### 编译

终于完成了繁琐的项目创建和配置，此时按下F7，VS就会开始自动编译创建项目。显示如下内容说明编译成功：

```
1>------ 已启动生成:  项目: vnltsmd, 配置: Release Win32 ------
1>  dllmain.cpp
1>  stdafx.cpp
1>  vnltsmd.cpp
1>     正在创建库 D:\vnltsmd\Release\vnltsmd.lib 和对象 D:\vnltsmd\Release\vnltsmd.exp
1>  正在生成代码
1>  已完成代码的生成
1>  vnltsmd.vcxproj -> D:\vnltsmd\Release\vnltsmd.dll
========== 生成:  成功 1 个，失败 0 个，最新 0 个，跳过 0 个 ==========
```

这时候我们到D:\vnltsmd\Release\文件夹下，找到vnltsmd.dll这个文件，将后缀名从.dll改为.pyd，就可以直接在python中导入使用了

从Github上下载的文件解压缩后的文件夹内，找到vnpy\vnpy\api\lts\vnltsmd\test文件夹，将改名后的vnltsmd.pyd复制到该文件夹下覆盖原本的同名文件，就可以使用mdtest.py进行测试了，注意要在mdtest.py中先填入你的LTS用户名和密码
