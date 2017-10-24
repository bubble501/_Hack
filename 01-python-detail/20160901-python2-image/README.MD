## 说明

在Web开发中经常用到验证码，为了防止机器人注册或者恶意登录和查询等，作用不容小觑

但验证码其实不是一个函数就能搞定的，它需要生成图片和水印，其实每种语言都有相关的函数

Python生成验证码主要用到以下模块

* Image
* ImageDraw
* ImageFont
* ImageFilter
* 随机数生成模块random

初步的API的使用规范可以看代码中的注释说明

更详细的可以通过文章[《Python PIL ImageDraw 和ImageFont模块学习 》](http://blog.csdn.net/dou_co/article/details/17618319)学习

## 实验步骤

* 编写代码
* 运行`python image.py`
* 然后即可在目录下生成验证码图片，例如

![image](./image/validate.gif)
