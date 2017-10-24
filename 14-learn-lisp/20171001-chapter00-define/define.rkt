#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; racket没有定义下面基础的函数
; 所以需要自己定义这些基础的函数以方便使用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; atom? 用于判断一个对象是不是原子
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; 测试
(eq? #t (atom? 'a))
(eq? #t (atom? '1))
(eq? #f (atom? '(1 2)))


; 用于判断传入的参数是不是S表达式都是原子的或者空的列表
; 明显，我的实现没有遵循第一诫
(define my-lat?
  (lambda (x)
    (or (eq? x '()) (and (pair? x) (atom? (car x)) (my-lat? (cdr x))))))

; 测试
(eq? #t (my-lat? '()))
(eq? #t (my-lat? '(a b c)))
(eq? #f (my-lat? '((a b c) d e f)))

; 《The Little Schemer》中的lat? 定义，使用cond和递归
(define lat?
  (lambda(l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; 测试
(eq? #t (lat? '()))
(eq? #t (lat? '(a b c)))
(eq? #f (lat? '((a b c) d e f)))


; 判断a是不是lat的成员
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

; 测试
(eq? #f (member? 'a '()))
(eq? #t (member? 'a '(a b c)))
(eq? #t (member? 'd '((a b c) d e f)))


; 将一个原子a和一个列表lat作为参数
; 生成一个新列表，新列表为移除首个a的lat
(define rember
  (lambda (a lat)
    (cond
      ; Scheme第一诫: 在表述任意函数时，总是先问null?
      ((null? lat) '())
      (else
       (cond
         ((eq? a (car lat)) (cdr lat))
         ; Scheme第二诫: 用cons构建列表
         (else (cons (car lat) (rember a (cdr lat)))))))))

; 测试
(rember 'a '(a a b c d))
'(a b c d)


; 以一个列表l为参数，该列表要么为空列表，要么只包含非空列表
; 该函数构建出一个新列表，新列表的元素由列表l中每个内部列表的第一个S-表达式组成
(define firsts
  (lambda (l)
    (cond
      ; Scheme第一诫：在表述任意函数时，总是先问null?
      ((null? l) '())
      ; Scheme第二诫: 用cons构建列表
      ; Scheme第三诫: 构建列表时，描述第一个典型元素，之后cons该元素到一般性递归上
      (else (cons (first (car l)) (firsts (cdr l)))))))
      ; 或者不要first而是用car
      ; (else (cons (car (car l)) (firsts (cdr l)))))))

; 测试firsts，需要是下面这样
(firsts '((1 2 3) (4 5 6)))
'(1 4)
; 不能是下面这样（注意'的使用）
; (firsts '('(1 2 3) '(4 5 6)))


; 三个参数: 原子new和old、列表lat
; 在lat的第一个old后插入new，并返回新列表
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (cons new '()))
      ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

; 测试insertR
(insertR 'a 'b '(1 2 3 4 b e f))
'(1 2 3 4 b a e f)
(insertR 'a 'b '())
'(a)
(insertR 'a 'b '(1 2 b 3 4 b 5 6 b))
'(1 2 b a 3 4 b 5 6 b)