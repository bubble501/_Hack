assume cs:codesg

codesg segment
       mov ax, 4c00h
       int 21h

; 程序从start的地方开始执行
start: mov ax, 0
    s: nop  ; jmp short s1
       nop  ; nop

       mov di, offset s    ; di中是s在代码段的偏移地址
       mov si, offset s2   ; si中是s2在代码段的偏移地址
       mov ax, cs:[si]     ; ax中是标号s2的地址
       mov cs:[di], ax     ; 将s2处的指令拷贝到s所在的地址处

   s0: jmp short s

   s1: mov ax, 0
       int 21h
       mov ax, 0

   s2: jmp short s1
       nop

codesg ends
end start