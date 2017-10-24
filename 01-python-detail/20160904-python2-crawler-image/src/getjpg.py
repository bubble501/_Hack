#coding=utf-8

#urllib模块提供了读取Web页面数据的接口，可以像读取本地文件一样读取www和ftp上的数据
import urllib

#re是正则表达式处理模块
import re


#向getHtml()传入一个URL地址，把该URL对应的页面下载下来
def getHtml(url):
    page = urllib.urlopen(url)  #打开一个URL
    html = page.read()          #下载URL上对应的HTML数据
    return html

#getImg()用于在获取的整个页面中山选需要的图片链接并下载
def getImg(html):
    reg = r'src="(.+?\.jpg)" pic_ext'       #该URL中对应的图片链接的正则表达式格式
    imgre = re.compile(reg)                 #re.compile()可以把正则表达式编译成一个正则表达式对象
    imglist = re.findall(imgre,html)        #读取HTML中包含imgre(正则表达式)的数据
    x = 0
    #循环把筛选的图片地址下载并保存到本地
    for imgurl in imglist:
        #urllib.urlretrieve()用于将远程数据下载到本地
        #第一个参数指定远程数据的url地址
        #第二个参数指定下载到本地后保存的地址、名称
        urllib.urlretrieve(imgurl, './%s.jpg' % x)     
        x+=1
    return imglist


if __name__ == '__main__':
    html = getHtml("http://tieba.baidu.com/p/2460150866")
    imageList = getImg(html)

    #以追加方式打开'rul.txt'文件，如果该文件不存在则自动创建
    fp = open('url.txt', 'w+')
    for imageUrl in imageList:
        #循环将所有图片的URL信息写到url.txt文件中
        fp.write(imageUrl + '\n')
    fp.close()
