assume cs:codesg

datasg segment
  db 'welcome to masm!'
datasg ends

codesg segment
start:  mov ax, datasg
        mov ds, ax
        ;使用段寄存器来访问B8000H-BFFFFH这段内存空间
        mov ax, 0B800H   ; 当16进制数第一位是字符时，前面必须加0，否则无法编译
        mov ss, ax
        
        ;第一行为绿色
        mov cx, 10H
        mov si, 0
        mov di, 0
    s1: mov al, ds:[si]
        mov byte ptr ss:[di], al               ;ASCII
        mov byte ptr ss:[di+1], 00000010B      ;属性：绿字
        inc si
        add di, 2
        loop s1

        ;第二行为绿底红色
        mov cx, 10H
        mov si, 0
        mov di, 0
    s2: mov al, ds:[si]
        mov byte ptr ss:[di+20H], al           ;ASCII
        mov byte ptr ss:[di+1+20H], 00100100B  ;属性：绿底红字
        inc si
        add di, 2
        loop s2

        ;第三行为白底蓝色
        mov cx, 10H
        mov si, 0
        mov di, 0
    s3: mov al, ds:[si]
        mov byte ptr ss:[di+40H], al           ;ASCII
        mov byte ptr ss:[di+1+40H], 01110001B  ;属性：白底蓝字
        inc si
        add di, 2
        loop s3

        mov ax, 4c00H
        int 21H
codesg ends
end start