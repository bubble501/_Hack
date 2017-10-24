# coding:utf-8

# 将所有收集好的数据写入到一个html文件中

class HtmlOutputer(object):
    def __init__(self):
        self.datas = []      #创建一个列表来维护收集的数据

    # 收集数据
    def collect_data(self, data):
        if data is None:
            return
        self.datas.append(data)

    # 将收集好的数据输出到一个html页面中
    def output_html(self):
        fout = open('output.html', 'w')     #创建一个文件的输出对象
        
        fout.write("<html>")
        fout.write("<head>")
        fout.write('<meta charset="utf-8"></meta>')
        fout.write("</head>")

        fout.write("<body>")
        fout.write("<table>")
        
        for data in self.datas:
            fout.write("<tr>")
            #python的默认编码是ascii，所以需要转换成utf-8，否则输出的中文可能是乱码
            fout.write("<td>%s</td>" % data['url'].encode('utf-8'))
            fout.write("<td>%s</td>" % data['title'].encode('utf-8'))
            fout.write("<td>%s</td>" % data['summary'].encode('utf-8'))
            fout.write("</tr>")

        fout.write("</table>")
        fout.write("</body>")
        fout.write("</html>")

        fout.close()
