# coding:utf-8

# html的解析器

from bs4 import BeautifulSoup
import re
import urlparse

class HtmlParser(object):
    # 获取当前页面中的词条url列表
    def _get_new_urls(self, page_url, soup):
        #用一个集合存储本html中所有的词条url
        new_urls = set()

        #需要匹配的url格式： /view/数字.htm
        links = soup.find_all('a', href=re.compile(r"/view/\d+\.htm"))
        for link in links:
            new_url = link['href']                              #此时获取的是不完全的url，比如是：/view/123.htm
            new_full_url = urlparse.urljoin(page_url, new_url)  #此时获取的是：http://baike.baidu.com/view/21087.htm
            new_urls.add(new_full_url)
        return new_urls

    # 解析数据，需要解析标题和简介两个数据
    def _get_new_data(self, page_url, soup):
        #用一个字典存储标题和简介数据
        res_data = {}

        #url也放到字典中，方便我们的使用
        res_data['url'] = page_url

        #标题对应的html如下：<dd class="lemmaWgt-lemmaTitle-title"> <h1>Python</h1>
        title_node = soup.find('dd', class_="lemmaWgt-lemmaTitle-title").find('h1')
        res_data['title'] = title_node.get_text()

        #简介对应的html如下：<div class="lemma-summary" label-module="lemmaSummary">
        summary_node = soup.find('div', class_="lemma-summary")
        res_data['summary'] = summary_node.get_text()

        return res_data

    # 解析html数据
    # 第一个参数表示该html对应的url；第二个参数表示html页面内容
    def parse(self, page_url, html_cont):
        if (page_url is None) or (html_cont is None):
            return

        soup = BeautifulSoup(html_cont, 'html.parser', from_encoding='utf-8')
        new_urls = self._get_new_urls(page_url, soup)
        new_data = self._get_new_data(page_url, soup)
        return new_urls, new_data
