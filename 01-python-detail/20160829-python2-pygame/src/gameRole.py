# -*- coding: utf-8 -*-

import pygame

SCREEN_WIDTH = 480
SCREEN_HEIGHT = 800

TYPE_SMALL = 1
TYPE_MIDDLE = 2
TYPE_BIG = 3

#说明
# 1.游戏中所有的可视化类需要继承自pygame.sprite.Sprite，pygame.sprite.Sprite就是pygame中用来实现精灵的类
# 2.当前的实现中对于子弹、飞机、敌人的速度都指定了，正常应该在界面上提供速度的可配置项

#子弹类
class Bullet(pygame.sprite.Sprite):
    #指定子弹的图片、在游戏主窗体的位置来初始化子弹对象
    def __init__(self, bullet_img, init_pos):
        pygame.sprite.Sprite.__init__(self)
        self.image = bullet_img
        self.rect = self.image.get_rect()
        self.rect.midbottom = init_pos
        self.speed = 10

    def move(self):
        self.rect.top -= self.speed

#玩家类，也就是飞机类
class Player(pygame.sprite.Sprite):
    def __init__(self, plane_img, player_rect, init_pos):
        pygame.sprite.Sprite.__init__(self)
        self.image = []                         #存储玩家对象精灵图片的列表
        for i in range(len(player_rect)):
            self.image.append(plane_img.subsurface(player_rect[i]).convert_alpha())
        self.rect = player_rect[0]              #初始化图片所在的矩形
        self.rect.topleft = init_pos            #初始化矩形的左上角坐标
        self.speed = 8                          #初始化玩家速度，这里是确定值
        self.bullets = pygame.sprite.Group()    #玩家飞机所发射的子弹的集合
        self.img_index = 0                      #玩家精灵图片索引
        self.is_hit = False                     #玩家是否被击中

    #发射一颗子弹
    def shoot(self, bullet_img):
        bullet = Bullet(bullet_img, self.rect.midtop)
        self.bullets.add(bullet)

    #向上移动
    def moveUp(self):
        if self.rect.top <= 0:
            self.rect.top = 0
        else:
            self.rect.top -= self.speed

    #向下移动
    def moveDown(self):
        if self.rect.top >= SCREEN_HEIGHT - self.rect.height:
            self.rect.top = SCREEN_HEIGHT - self.rect.height
        else:
            self.rect.top += self.speed

    #向左移动
    def moveLeft(self):
        if self.rect.left <= 0:
            self.rect.left = 0
        else:
            self.rect.left -= self.speed

    #向右移动
    def moveRight(self):
        if self.rect.left >= SCREEN_WIDTH - self.rect.width:
            self.rect.left = SCREEN_WIDTH - self.rect.width
        else:
            self.rect.left += self.speed

#敌人类
class Enemy(pygame.sprite.Sprite):
    def __init__(self, enemy_img, enemy_down_imgs, init_pos):
        pygame.sprite.Sprite.__init__(self)
        self.image = enemy_img
        self.rect = self.image.get_rect()
        self.rect.topleft = init_pos
        self.down_imgs = enemy_down_imgs
        self.speed = 2
        self.down_index = 0

    def move(self):
        self.rect.top += self.speed
