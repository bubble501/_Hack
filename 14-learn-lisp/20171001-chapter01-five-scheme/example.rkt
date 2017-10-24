#lang racket

; 这里是展示Scheme一些基础的语法
; 熟悉Scheme的基础语法、函数


; 基本元件car仅定义为针对非空列表，获取列表中的第一个S表达式
(car '((a b c) x y z))
; 输出: '(a b c)
(car '(x y z))
; 输出: 'x


; cdr仅定义于非空列表，获取列表l扣除(car l)的部分
(cdr '((a b c) x y z))
; 输出: '(x y z)
(cdr '((a b c) x))
; 输出: '(x)


; cons第一个参数是S表达式，第二个参数是列表
; 将第一个参数放到第二个参数列表中，返回一个列表
(cons 'a '(1 2))
; 输出: '(a 1 2)
(cons '(1 2) '(a b c))
; 输出: '((1 2) a b c)


; null?判断列表是否为空
(null? 'a)
; #f
(null? '())
; #t
(null? '(1 2))
; #f


; eq?判断两个非数字原子是否相等
; eq?需两个参数，每个参数必须是一个非数字原子
(eq? 'abc 'abc)
; 输出: #t
(eq? 1 1)
; 输出: #t
(eq? '1 '1)
; 输出: #t
(eq? '() '())
; 输出: #t
(eq? '(1 2) '(1 2))
; 输出: #f