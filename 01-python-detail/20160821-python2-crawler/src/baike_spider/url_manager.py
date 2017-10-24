# coding:utf-8

# 本python脚本是url管理器

class UrlManager(object):
    def __init__(self):
        self.new_urls = set()   #新的还未被爬取的url集合，使用集合进行管理
        self.old_urls = set()   #已经被爬取过的url集合，也是使用集合进行管理

    # 向管理器中添加一个新的url
    def add_new_url(self, url):
        if url is None:
            return
        if (url not in self.new_urls) and (url not in self.old_urls):
            self.new_urls.add(url)
    
    # 向管理器中添加批量url
    def add_new_urls(self, urls):
        if (urls is None) or (0 == len(urls)):
            return
        for url in urls:
            self.add_new_url(url)

    # 判断管理器中是否存在还未爬取的url
    def has_new_url(self):
        return (0 != len(self.new_urls))

    # 从url管理器中获取一个新的待爬取的url
    def get_new_url(self):
        new_url = self.new_urls.pop()       #pop方法是从集合中获取一个元素，并且将其从集合中移除
        self.old_urls.add(new_url)
        return new_url
