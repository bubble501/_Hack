# coding:utf-8

# 本python脚本是爬虫总调度程序
# 以一个入口url作为参数来爬取所有相关页面

from baike_spider import url_manager, html_downloader, html_parser, html_outputer

class SpiderMain(object):
    def __init__(self):
        self.urls = url_manager.UrlManager()                # url管理器
        self.downloader = html_downloader.HtmlDownloader()  # html下载器
        self.parser = html_parser.HtmlParser()              # html解析器
        self.outputer = html_outputer.HtmlOutputer()        # html输出器

    # 爬虫的调度程序
    def craw(self, root_url):
        count = 1       #记录当前爬取的是第几个url
        
        #将入口url添加到url管理器
        self.urls.add_new_url(root_url) 

        #启动爬虫循环
        while self.urls.has_new_url():
            try:
                new_url = self.urls.get_new_url()                           #获取一个新的url
                print 'craw %d : %s' % (count, new_url)
                html_cont = self.downloader.download(new_url)               #下载新的url对应的页面，下载好的页面数据放到html_cont中
                new_urls, new_data = self.parser.parse(new_url, html_cont)  #解析下载好的html页面的数据，获取新的url列表、获取新的数据
                self.urls.add_new_urls(new_urls)                            #将新的url列表批量添加到url管理器
                self.outputer.collect_data(new_data)                        #收集数据
        
                if 1000 <= count:
                    break
                count = count + 1
            except Exception, e:
                #因为百度百科很多页面要么没有简介数据，要么某个url已经无法访问，所以需要对其进行异常保护，提升程序的稳健性
                print 'craw failed'
                
        #输出收集好的数据
        self.outputer.output_html()
    
if __name__ == '__main__':
    
    #入口url
    root_url = 'http://baike.baidu.com/view/21087.htm'
    
    #创建一个spider
    obj_spider = SpiderMain()
    #调用craw方法，来启动爬虫
    obj_spider.craw(root_url)
