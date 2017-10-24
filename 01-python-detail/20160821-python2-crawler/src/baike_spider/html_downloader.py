# coding:utf-8

# html下载器

import urllib2

class HtmlDownloader(object):
    # 下载url指向的页面html内容
    def download(self, url):
        if url is None:
            return None

        # 使用urllib2模块下载url指向的html内容
        response = urllib2.urlopen(url)
        # 如果http的返回码不是200，说明请求下载失败
        if 200 != response.getcode():
            return None
        # 否则返回下载好的内容
        return response.read()
