#!/usr/bin/env python
# -*- coding: utf-8 -*-

import random
import Image, ImageDraw, ImageFont, ImageFilter

_letter_cases = "abcdefghjkmnpqrstuvwxy"    #小写字母，去除可能干扰的i、l、o、z
_upper_cases = _letter_cases.upper()        #大写字母
_numbers = ''.join(map(str, range(3, 10)))  #数字

#map()是对range(3, 10)中的每个元素做str()操作，返回结果的list
# >>> map(str, range(3, 10))
#返回值['3', '4', '5', '6', '7', '8', '9']

#join()用于将序列中的元素以指定的字符连接生成一个新的字符串
# >>> str = "-"
# >>> seq = ("a", "b", "c");
# >>> print str.join(seq)
#最后输出的结果是 a-b-c

init_chars = ''.join((_letter_cases, _upper_cases, _numbers))
fontType = "/usr/share/fonts/truetype/freefont/FreeSans.ttf"

#生成验证码图片
#return[0] PIL Image实例
#return[1] 验证码图片中的字符串
def create_validate_code(size=(400, 150),        #图片的大小，格式(宽,高)
                        chars=init_chars,       #允许的字符集合，格式字符串
                        img_type="GIF",         #图片保存的格式，默认是GIF，可选的位GIT、JPEG、TIFF、PNG
                        mode="RGB",             #图片模式，默认是RGB
                        bg_color=(255,255,255), #背景颜色，默认是白色
                        fg_color=(0, 0, 255),   #前景色，验证码字符颜色，默认是蓝色#0000ff
                        font_size=50,           #验证码字体大小
                        font_type=fontType,     #验证码字体，默认是ae_AlArabiya.ttf
                        length=4,               #验证码字符个数
                        draw_lines=True,        #是否划干扰线
                        n_line=(1,2),           #干扰线的条数范围，格式元组，默认是(1,2)，只有draw_lines位True时才有效
                        draw_points=True,       #是否画干扰点
                        point_chance=2):        #干扰点出现的概率，大小范围是[0, 100]
    width, height = size                    #宽、高
    img = Image.new(mode, size, bg_color)   #创建图形
    draw = ImageDraw.Draw(img)              #创建画笔
    if draw_lines:
        #画干扰线
        create_lines(draw, n_line, width, height)
    if draw_points:
        #画干扰点
        create_points(draw, point_chance, width, height)
    strs = create_strs(draw, chars, length, font_type, font_size, width, height, fg_color)

    #图形扭曲参数
    params = [1 - float(random.randint(1, 2)) / 100,
              0,
              0,
              0,
              1 - float(random.randint(1, 10)) /100,
              float(random.randint(1, 2)) / 500, 
              0.001,
              float(random.randint(1, 2))/ 500]
    #创建扭曲
    img = img.transform(size, Image.PERSPECTIVE, params)
    #滤镜，边界加强（阀值更大）
    img = img.filter(ImageFilter.EDGE_ENHANCE_MORE)

    return img, strs

def create_lines(draw, n_line, width, height):
    '''绘制干扰线'''
    line_num = random.randint(n_line[0], n_line[1])     #干扰线条数
    for i in range(line_num):
        #随机在图片平面范围内生成一个起始点
        begin = (random.randint(0, width), random.randint(0, height))
        #随机在图片平面范围内生成一个结束点
        end = (random.randint(0, width), random.randint(0, height))
        
        #用画笔在图片上绘制线
        # 第一个参数指定线的起始点和结束点
        # fill参数表示绘制的线的颜色，用RGB元组表示
        draw.line([begin, end], fill=(255, 0, 0))

def create_points(draw, point_chance, width, height):
    '''绘制干扰点'''
    chance = min(100, max(0, int(point_chance)))        #大小限制在[0,100]

    for w in xrange(width):
        for h in xrange(height):
            #根据指定的点出现的概率在图片范围内生成点
            tmp = random.randint(0, 100)
            if tmp > 100 - chance:
                #用画笔在图片上绘制点
                # 第一个参数指定点的像素坐标
                # fill参数表示绘制的点的颜色，用RGB元组表示
                draw.point((w, h), fill = (0, 255, 0))

def create_strs(draw, chars, length, font_type, font_size, width, height, fg_color):
    '''绘制验证码字符'''
    '''生成给定长度的字符串，返回列表格式'''
    c_chars = random.sample(chars, length)
    strs = ' %s ' % ' '.join(c_chars)       #每个字符前后以空格隔开

    font = ImageFont.truetype(font_type, font_size)
    font_width, font_height = font.getsize(strs)

    #用画笔在图片上添加文字
    # 第一个参数是二元元组，指定字符串左上角坐标
    draw.text(((width - font_width) / 5, (height - font_height) / 5), strs, font = font, fill = fg_color)
    return ''.join(c_chars)

if __name__ == '__main__':
    #create_validate_code方法返回两个参数
    # 第一个是Image实例：           code_img[0]
    # 第二个参数是图片对应的字符：  code_ime[1]
    code_img = create_validate_code()                   
    code_img[0].save(code_img[1] + ".gif", "GIF")       #调用其save方法可以在本地生成图片,图片命名为对应的字符串
    print code_img[1]                                   #在实际应用中可以拿这个值和用户输入的验证码进行对比
