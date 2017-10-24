assume cs:codesg

data segment
  db '1975','1976','1977','1978','1979','1980','1981','1982','1983'
  db '1984','1985','1986','1987','1988','1989','1990','1991','1992'
  db '1993','1994','1995'
  ; 以上是表示21年的21个字符串

  dd 16,22,382,1356,2390,8000,16000,24486,50065,97479,140417,197514
  dd 345980,590827,803530,1183000,1843000,2759000,3753000,4649000,5937000
  ; 以上是表示21年公司总收入的21个dword数据

  dw 3,7,9,13,28,38,130,220,476,778,1001,1442,2258,2793,4037,5635,8226
  dw 11542,14430,15257,17800
  ; 以上是表示21年公司雇员人数的21个word型数据
data ends

table segment
  db 21 dup ('year summ ne ?? ')
table ends

codesg segment

start:   mov ax, data
         mov ds, ax
         mov ax, table
         mov ss, ax

         mov cx, 15H
         mov si, 0
         mov bx, 0
         mov di, 0

s0:      ; 将年份(4字节)从data拷贝到table
         mov ax, ds:[di]
         mov ss:[si], ax
         mov ax, ds:[di+2]
         mov ss:[si+2], ax
         ;写入空格
         mov byte ptr ss:[si+4], ' '

         ; 将收入(4字节)从data拷贝到table
         mov ax, ds:[di+84]
         mov ss:[si+5], ax
         mov ax, ds:[di+2+84]
         mov ss:[si+7], ax
         ;写入空格
         mov byte ptr ss:[si+9], ' '

         ;将雇员数(2字节)从data拷贝到table
         mov ax, ds:[bx+84+84]
         mov ss:[si+10], ax
         ;写入空格
         mov byte ptr ss:[si+12], ' '

         ;计算人均收入
         ;将32位收入拷贝到AX、DX
         mov ax, ds:[di+84]     ;低16位
         mov dx, ds:[di+2+84]   ;高16位
         ;进行除运算
         div word ptr ds:[bx+84+84]
         ;将商拷贝到table段
         mov ss:[si+13], ax

         ;写入一行的最后一个空格
         mov byte ptr ss:[si+15], ' '

         ;切换到下一组记录
         add di, 4H
         add bx, 2H
         add si, 10H

         loop s0

         mov ax, 4C00H
         int 21H
codesg ends
end start