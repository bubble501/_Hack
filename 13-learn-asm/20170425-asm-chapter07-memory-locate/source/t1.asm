assume cs:codesg, ds:datasg

datasg segment
  db 'welcome to masm!'
  db '................'
datasg ends

codesg segment
start: mov ax, datasg
       mov ds, ax
       mov si, 0
       mov di, 10H
       mov cx, 10H      ;循环16次，因为'welcome to masm!'有16个字符
   s1: mov al, [si]    ;将'welcome to masm!'的字符逐个拷贝到al寄存器
       mov [di], al
       inc si
       inc di
       loop s1

       mov ax, 4c00H
       int 21H
codesg ends

end start