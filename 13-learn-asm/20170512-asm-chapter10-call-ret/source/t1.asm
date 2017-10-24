assume cs:code

data segment
    dw 1111H, 1111H, 1111H, 1111H, 1111H, 1111H, 1111H, 1111H
    dw 1111H, 1111H, 1111H, 1111H, 1111H, 1111H, 1111H, 1111H
data ends

code segment
start: 
    mov ax, data
    mov ds, ax
    mov si, 0
    mov di, 8
    call add128

    mov ax, 4C00H
    int 21H

add128: 
    push ax
    push cx
    push si
    push di

    sub ax, ax ; 将CF设置为0

    mov cx, 8
s: 
    mov ax, [si]
    adc ax, [di]   ;adc ax, bx ==> ax = ax + bx + cf
    mov [si], ax
    inc si
    inc si
    inc di
    inc di
    loop s

    pop di
    pop si
    pop cx
    pop ax

    ret

code ends
end start