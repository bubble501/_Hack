# -*- coding: utf-8 -*-

#导入pygame库及需要的库
import pygame
from pygame.locals import *

from sys import exit

#导入封装的子弹、飞机、敌机类
from gameRole import *
import random

#初始化游戏
pygame.init()
#设置游戏窗口的大小，并生成游戏窗口
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
#设置窗体标题
pygame.display.set_caption('飞机大战')

#载入音乐，pygame.mixer是一个用来处理声音的模块
bullet_sound = pygame.mixer.Sound('resources/sound/bullet.wav')             #子弹声音
enemy1_down_sound = pygame.mixer.Sound('resources/sound/enemy1_down.wav')   #敌机坠毁声音
game_over_sound = pygame.mixer.Sound('resources/sound/game_over.wav')       #游戏结束声音
#set_volume()用来设置音量，音量的范围在0~1.0之间
bullet_sound.set_volume(0.3)
enemy1_down_sound.set_volume(0.3)
game_over_sound.set_volume(0.3)
pygame.mixer.music.load('resources/sound/game_music.wav')
pygame.mixer.music.play(-1, 0.0)
pygame.mixer.music.set_volume(0.25)

#载入背景图
background = pygame.image.load('resources/image/background.png').convert()  #正常游戏过程中的背景图
game_over = pygame.image.load('resources/image/gameover.png')               #游戏结束时的背景图

#载入飞机图片
filename = 'resources/image/shoot.png'
plane_img = pygame.image.load(filename)

#设置玩家相关参数
player_rect = []
#所有飞机图片都在shoot.png一张图片中，下面指定上下左右像素坐标以抠出各个飞机图片
#这里就需要精确地指定大图中每个小兔所在的区域，严格精确到每一个像素
#pygame.Rect()指定一个矩形对象，前两个参数指定左上左边，后两个参数指定右下坐标
player_rect.append(pygame.Rect(0, 99, 102, 126))    #玩家精灵图片区域 
player_rect.append(pygame.Rect(165, 360, 102, 126))
player_rect.append(pygame.Rect(165, 234, 102, 126)) #玩家爆炸精灵图片区域
player_rect.append(pygame.Rect(330, 624, 102, 126))
player_rect.append(pygame.Rect(330, 498, 102, 126))
player_rect.append(pygame.Rect(432, 624, 102, 126))
player_pos = [200, 600]
player = Player(plane_img, player_rect, player_pos)

#定义子弹对象使用的surface相关参数
#什么是surface？我们在背景上显示的元素(飞机、子弹)在pygame中就是一个surface
#subsurface方法的一种用法：首先load一张大图，然后调用subsurface方法获取大图中的一小部分，并对应生成一个小图
bullet_rect = pygame.Rect(1004, 987, 9, 21)
bullet_img = plane_img.subsurface(bullet_rect)

#定义敌机对象使用的surface相关参数
enemy1_rect = pygame.Rect(534, 612, 57, 43)
enemy1_img = plane_img.subsurface(enemy1_rect)
enemy1_down_imgs = []
enemy1_down_imgs.append(plane_img.subsurface(pygame.Rect(267, 347, 57, 43)))
enemy1_down_imgs.append(plane_img.subsurface(pygame.Rect(873, 697, 57, 43)))
enemy1_down_imgs.append(plane_img.subsurface(pygame.Rect(267, 296, 57, 43)))
enemy1_down_imgs.append(plane_img.subsurface(pygame.Rect(930, 697, 57, 43)))
enemies1 = pygame.sprite.Group()

#存储被击毁的飞机，用来渲染击毁精灵画面
enemies_down = pygame.sprite.Group()

shoot_frequency = 0
enemy_frequency = 0

player_down_index = 16

score = 0

clock = pygame.time.Clock()

running = True

while running:
    #控制游戏最大帧率为60
    clock.tick(60)

    #控制发射子弹频率，并发射子弹
    if not player.is_hit:
        if shoot_frequency % 15 == 0:
            bullet_sound.play()         #响起子弹对应的声音
            player.shoot(bullet_img)    #飞机发射子弹
        shoot_frequency += 1
        if shoot_frequency >= 15:
            shoot_frequency = 0

    #生成敌机
    if enemy_frequency % 50 == 0:
        enemy1_pos = [random.randint(0, SCREEN_WIDTH - enemy1_rect.width), 0]
        enemy1 = Enemy(enemy1_img, enemy1_down_imgs, enemy1_pos)
        enemies1.add(enemy1)
    enemy_frequency += 1
    if enemy_frequency > 100:
        enemy_frequency = 0

    #移动子弹，若超出窗口范围则删除
    for bullet in player.bullets:
        bullet.move()
        if bullet.rect.bottom < 0:
            player.bullets.remove(bullet)

    #移动敌机，若超出窗口范围则删除
    for enemy in enemies1:
        enemy.move()
        #判断玩家是否被击中
        #pygame.sprite.collide_circle()用于进行圆形冲突检测
        #这个函数是基于每个精灵的半径值来进行检测的
        #除了圆形冲突检测，还有collide_rect、collide_rect_ratio等
        if pygame.sprite.collide_circle(enemy, player):
            enemies_down.add(enemy)
            enemies1.remove(enemy)
            player.is_hit = True
            game_over_sound.play()      #如果判断给击中，则响起结束音乐
            break
        if enemy.rect.top > SCREEN_HEIGHT:
            enemies1.remove(enemy)

    #将被击中的敌机对象添加到击毁敌机的Group中，用来渲染击毁动画
    enemies1_down = pygame.sprite.groupcollide(enemies1, player.bullets, 1, 1)
    for enemy_down in enemies1_down:
        enemies_down.add(enemy_down)

    #绘制背景
    screen.fill(0)
    screen.blit(background, (0, 0))

    #绘制玩家飞机
    if not player.is_hit:
        screen.blit(player.image[player.img_index], player.rect)
        #更换图片索引使飞机有动画效果
        player.img_index = shoot_frequency // 8
    else:
        player.img_index = player_down_index // 8
        #如果玩家飞机被击毁，那么调用screen.blit()方法在原来玩家飞机所在的位置上，用爆炸的图片替换正常的图片，绘制在屏幕上
        #screen.blit()的第一个参数表示要被绘制的东西，这里就是一个图像对象；第二个参数表示要绘制的矩形像素区域
        screen.blit(player.image[player.img_index], player.rect)
        player_down_index += 1
        if player_down_index > 47:
            running = False

    #绘制击毁动画
    #enemies_down中存储的是所有被击毁的敌机对象
    for enemy_down in enemies_down:
        if enemy_down.down_index == 0:
            enemy1_down_sound.play()
        if enemy_down.down_index > 7:
            enemies_down.remove(enemy_down)
            score += 1000
            continue
        #blit()方法的第一个参数是pygame.image.load()获得的对象，或者font.render()获得的对象，或者其他
        #blit()方法的第二个参数是pygame.Rect()获得的对象，用于指定一个可视化控件的矩形位置对象，前两个参数指定左上坐标，后两个参数指定右下坐标
        screen.blit(enemy_down.down_imgs[enemy_down.down_index // 2], enemy_down.rect)
        enemy_down.down_index += 1

    #绘制子弹和敌机
    #在屏幕(screen)中绘制子弹(player.bullets)和敌机(enemies1)
    player.bullets.draw(screen)
    enemies1.draw(screen)

    #绘制得分
    score_font = pygame.font.Font(None, 36)
    score_text = score_font.render(str(score), True, (128, 128, 128))
    text_rect = score_text.get_rect()
    text_rect.topleft = [10, 10]
    screen.blit(score_text, text_rect)

    #更新屏幕
    pygame.display.update()

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            exit()

    #监听键盘事件
    key_pressed = pygame.key.get_pressed()
    #若玩家被击中，则无效
    if not player.is_hit:
        if key_pressed[K_w] or key_pressed[K_UP]:
            player.moveUp()
        if key_pressed[K_s] or key_pressed[K_DOWN]:
            player.moveDown()
        if key_pressed[K_a] or key_pressed[K_LEFT]:
            player.moveLeft()
        if key_pressed[K_d] or key_pressed[K_RIGHT]:
            player.moveRight()

font = pygame.font.Font(None, 48)
text = font.render('Score: ' + str(score), True, (255, 0, 0))
text_rect = text.get_rect()
text_rect.centerx = screen.get_rect().centerx
text_rect.centery = screen.get_rect().centery + 24
#游戏结束，背景图片更新为game_over图片
screen.blit(game_over, (0, 0))
#游戏结束，计算总得分
screen.blit(text, text_rect)

while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            exit()
    pygame.display.update()
