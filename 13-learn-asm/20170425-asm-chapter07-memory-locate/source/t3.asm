assume cs:codesg, ds:datasg, ss:stacksg

datasg segment
  db 'ibm             '
  db 'dec             '
  db 'dos             '
  db 'vax             '
datasg ends

;定义栈段，容量为16字节
stacksg segment
  dw 0, 0, 0, 0, 0, 0, 0, 0
stacksg ends

codesg segment
start: mov ax, stacksg
       mov ss, ax
       mov sp, 16
       mov ax, datasg
       mov ds, ax
       mov bx, 0

       mov cx, 4
   s0: push cx           ;将外层循环的cx值压栈
       mov si, 0
       mov cx, 3         ;将cx设置为内层循环的计数

    s: mov al, [bx+si]
       and al, 11011111b
       mov [bx+si], al
       inc si
       loop s

       add bx, 16
       pop cx            ;从栈顶弹出原cx的值，恢复cx
       loop s0           ;外层循环的loop指令将cx的计数值减一

       mov ax, 4c00H
       int 21H
codesg ends

end start