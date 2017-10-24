使用GCC编译源代码，就能生成机器语言（指令代码）构成的二进制文件。在分析core dump或kernel dump时，通过gdb、crash等调试工具可以看到反汇编之后的汇编代码，而最终，必须在由C语言等写成的源代码中找出问题的所在

>要理解汇编语言，就必须了解CPU的寄存器和机器语言

先实现一个简单的C语言程序

```
#include <stdio.h>

int global;
int func_op(void) { return 0; }

void func(void)
{
	unsigned long long val64 = 0;

	val64 = 0xffffeeeeddddcccc;					//(7)	局部变量赋值
	global = 0x5555;							//(8)	全局变量赋值
}

#define MAX_WORD 16

int main(void)
{
	unsigned int i = 0;
	char words[MAX_WORD] = "Hello World";
	char word;

	int (*func_pointer)(void) = &func_op;

	i = 0xabcd;									//(1)	局部变量赋值

	if(i != 0x1234)								//(2)	判断语句
		i = 0;									//(3)

	while(i == 0)								//(4)	while循环语句
		i++;									//(5)

	func();										//(6)	函数调用
	i = func_pointer();							//(9)

	for(i=0; i<MAX_WORD-1; i++)					//(10)	for循环语句
		word = words[i];						//(11)

	return 0;									//(12)	函数返回
}
```

然后使用`gcc -Wall -O0 -m32 test.c -o test`编译程序

* -O0禁用gcc的优化选项
* -m32表示编译得到32位程序

然后使用`objdump -d --no-show-raw-insn test`进行反汇编，--no-show-raw-ins选项表示不输出机器语言，输出如下

```
test:     file format elf32-i386


Disassembly of section .init:

080482d0 <_init>:
 80482d0:	push   %ebx
 80482d1:	sub    $0x8,%esp
 80482d4:	call   8048370 <__x86.get_pc_thunk.bx>
 80482d9:	add    $0x1d27,%ebx
 80482df:	mov    -0x4(%ebx),%eax
 80482e5:	test   %eax,%eax
 80482e7:	je     80482ee <_init+0x1e>
 80482e9:	call   8048330 <__libc_start_main@plt+0x10>
 80482ee:	add    $0x8,%esp
 80482f1:	pop    %ebx
 80482f2:	ret    

Disassembly of section .plt:

08048300 <__stack_chk_fail@plt-0x10>:
 8048300:	pushl  0x804a004
 8048306:	jmp    *0x804a008
 804830c:	add    %al,(%eax)
	...

08048310 <__stack_chk_fail@plt>:
 8048310:	jmp    *0x804a00c
 8048316:	push   $0x0
 804831b:	jmp    8048300 <_init+0x30>

08048320 <__libc_start_main@plt>:
 8048320:	jmp    *0x804a010
 8048326:	push   $0x8
 804832b:	jmp    8048300 <_init+0x30>

Disassembly of section .plt.got:

08048330 <.plt.got>:
 8048330:	jmp    *0x8049ffc
 8048336:	xchg   %ax,%ax

Disassembly of section .text:

08048340 <_start>:
 8048340:	xor    %ebp,%ebp
 8048342:	pop    %esi
 8048343:	mov    %esp,%ecx
 8048345:	and    $0xfffffff0,%esp
 8048348:	push   %eax
 8048349:	push   %esp
 804834a:	push   %edx
 804834b:	push   $0x8048590
 8048350:	push   $0x8048530
 8048355:	push   %ecx
 8048356:	push   %esi
 8048357:	push   $0x8048474
 804835c:	call   8048320 <__libc_start_main@plt>
 8048361:	hlt    
 8048362:	xchg   %ax,%ax
 8048364:	xchg   %ax,%ax
 8048366:	xchg   %ax,%ax
 8048368:	xchg   %ax,%ax
 804836a:	xchg   %ax,%ax
 804836c:	xchg   %ax,%ax
 804836e:	xchg   %ax,%ax

08048370 <__x86.get_pc_thunk.bx>:
 8048370:	mov    (%esp),%ebx
 8048373:	ret    
 8048374:	xchg   %ax,%ax
 8048376:	xchg   %ax,%ax
 8048378:	xchg   %ax,%ax
 804837a:	xchg   %ax,%ax
 804837c:	xchg   %ax,%ax
 804837e:	xchg   %ax,%ax

08048380 <deregister_tm_clones>:
 8048380:	mov    $0x804a01f,%eax
 8048385:	sub    $0x804a01c,%eax
 804838a:	cmp    $0x6,%eax
 804838d:	jbe    80483a9 <deregister_tm_clones+0x29>
 804838f:	mov    $0x0,%eax
 8048394:	test   %eax,%eax
 8048396:	je     80483a9 <deregister_tm_clones+0x29>
 8048398:	push   %ebp
 8048399:	mov    %esp,%ebp
 804839b:	sub    $0x14,%esp
 804839e:	push   $0x804a01c
 80483a3:	call   *%eax
 80483a5:	add    $0x10,%esp
 80483a8:	leave  
 80483a9:	repz ret 
 80483ab:	nop
 80483ac:	lea    0x0(%esi,%eiz,1),%esi

080483b0 <register_tm_clones>:
 80483b0:	mov    $0x804a01c,%eax
 80483b5:	sub    $0x804a01c,%eax
 80483ba:	sar    $0x2,%eax
 80483bd:	mov    %eax,%edx
 80483bf:	shr    $0x1f,%edx
 80483c2:	add    %edx,%eax
 80483c4:	sar    %eax
 80483c6:	je     80483e3 <register_tm_clones+0x33>
 80483c8:	mov    $0x0,%edx
 80483cd:	test   %edx,%edx
 80483cf:	je     80483e3 <register_tm_clones+0x33>
 80483d1:	push   %ebp
 80483d2:	mov    %esp,%ebp
 80483d4:	sub    $0x10,%esp
 80483d7:	push   %eax
 80483d8:	push   $0x804a01c
 80483dd:	call   *%edx
 80483df:	add    $0x10,%esp
 80483e2:	leave  
 80483e3:	repz ret 
 80483e5:	lea    0x0(%esi,%eiz,1),%esi
 80483e9:	lea    0x0(%edi,%eiz,1),%edi

080483f0 <__do_global_dtors_aux>:
 80483f0:	cmpb   $0x0,0x804a01c
 80483f7:	jne    804840c <__do_global_dtors_aux+0x1c>
 80483f9:	push   %ebp
 80483fa:	mov    %esp,%ebp
 80483fc:	sub    $0x8,%esp
 80483ff:	call   8048380 <deregister_tm_clones>
 8048404:	movb   $0x1,0x804a01c
 804840b:	leave  
 804840c:	repz ret 
 804840e:	xchg   %ax,%ax

08048410 <frame_dummy>:
 8048410:	mov    $0x8049f10,%eax
 8048415:	mov    (%eax),%edx
 8048417:	test   %edx,%edx
 8048419:	jne    8048420 <frame_dummy+0x10>
 804841b:	jmp    80483b0 <register_tm_clones>
 804841d:	lea    0x0(%esi),%esi
 8048420:	mov    $0x0,%edx
 8048425:	test   %edx,%edx
 8048427:	je     804841b <frame_dummy+0xb>
 8048429:	push   %ebp
 804842a:	mov    %esp,%ebp
 804842c:	sub    $0x14,%esp
 804842f:	push   %eax
 8048430:	call   *%edx
 8048432:	add    $0x10,%esp
 8048435:	leave  
 8048436:	jmp    80483b0 <register_tm_clones>

0804843b <func_op>:
 804843b:	push   %ebp
 804843c:	mov    %esp,%ebp
 804843e:	mov    $0x0,%eax
 8048443:	pop    %ebp
 8048444:	ret    

;func函数的反汇编
08048445 <func>:
 8048445:	push   %ebp							
 8048446:	mov    %esp,%ebp
 8048448:	sub    $0x10,%esp
 804844b:	movl   $0x0,-0x8(%ebp)
 8048452:	movl   $0x0,-0x4(%ebp)
 8048459:	movl   $0xddddcccc,-0x8(%ebp)		;(7)	val64 = 0xffffeeeeddddcccc（局部变量通过ebp基数指针寄存器去寻址）
 8048460:	movl   $0xffffeeee,-0x4(%ebp)
 8048467:	movl   $0x5555,0x804a020			;(6)	global = 0x5555（全局变量的地址是在编译的时候就确定的）
 8048471:	nop
 8048472:	leave  
 8048473:	ret    

;main函数的反汇编
08048474 <main>:
 8048474:	lea    0x4(%esp),%ecx
 8048478:	and    $0xfffffff0,%esp
 804847b:	pushl  -0x4(%ecx)
 804847e:	push   %ebp
 804847f:	mov    %esp,%ebp
 8048481:	push   %ecx
 8048482:	sub    $0x24,%esp
 8048485:	mov    %gs:0x14,%eax
 804848b:	mov    %eax,-0xc(%ebp)
 804848e:	xor    %eax,%eax
 8048490:	movl   $0x0,-0x24(%ebp)
 8048497:	movl   $0x6c6c6548,-0x1c(%ebp)
 804849e:	movl   $0x6f57206f,-0x18(%ebp)
 80484a5:	movl   $0x646c72,-0x14(%ebp)
 80484ac:	movl   $0x0,-0x10(%ebp)
 80484b3:	movl   $0x804843b,-0x20(%ebp)
 80484ba:	movl   $0xabcd,-0x24(%ebp)			;(1)	i=0xabcd
 80484c1:	cmpl   $0x1234,-0x24(%ebp)			;(2)	if(i != 0x1234)，对应比较和跳转的指令
 80484c8:	je     80484d7 <main+0x63>			;(2)
 80484ca:	movl   $0x0,-0x24(%ebp)				;(3)	i=0
 80484d1:	jmp    80484d7 <main+0x63>			;(2)
 80484d3:	addl   $0x1,-0x24(%ebp)				;(5)	i++
 80484d7:	cmpl   $0x0,-0x24(%ebp)				;(4)	while(i == 0)
 80484db:	je     80484d3 <main+0x5f>			;(4)
 80484dd:	call   8048445 <func>				;(6)	func()
 80484e2:	mov    -0x20(%ebp),%eax
 80484e5:	call   *%eax						;(9)	i = func_pointer();
 80484e7:	mov    %eax,-0x24(%ebp)
 80484ea:	movl   $0x0,-0x24(%ebp)				;(10)	for(i=0;;)
 80484f1:	jmp    8048505 <main+0x91>
 80484f3:	lea    -0x1c(%ebp),%edx
 80484f6:	mov    -0x24(%ebp),%eax
 80484f9:	add    %edx,%eax
 80484fb:	movzbl (%eax),%eax					;(11)
 80484fe:	mov    %al,-0x25(%ebp)
 8048501:	addl   $0x1,-0x24(%ebp)				;(10)	for(;; i++)
 8048505:	cmpl   $0xe,-0x24(%ebp)				;(10)	for(; i<MAX_WORD ;)
 8048509:	jbe    80484f3 <main+0x7f>
 804850b:	mov    $0x0,%eax					;(12)	return 0;之前先准备返回值
 8048510:	mov    -0xc(%ebp),%ecx
 8048513:	xor    %gs:0x14,%ecx
 804851a:	je     8048521 <main+0xad>
 804851c:	call   8048310 <__stack_chk_fail@plt>
 8048521:	add    $0x24,%esp
 8048524:	pop    %ecx
 8048525:	pop    %ebp
 8048526:	lea    -0x4(%ecx),%esp
 8048529:	ret    
 804852a:	xchg   %ax,%ax
 804852c:	xchg   %ax,%ax
 804852e:	xchg   %ax,%ax

08048530 <__libc_csu_init>:
 8048530:	push   %ebp
 8048531:	push   %edi
 8048532:	push   %esi
 8048533:	push   %ebx
 8048534:	call   8048370 <__x86.get_pc_thunk.bx>
 8048539:	add    $0x1ac7,%ebx
 804853f:	sub    $0xc,%esp
 8048542:	mov    0x20(%esp),%ebp
 8048546:	lea    -0xf4(%ebx),%esi
 804854c:	call   80482d0 <_init>
 8048551:	lea    -0xf8(%ebx),%eax
 8048557:	sub    %eax,%esi
 8048559:	sar    $0x2,%esi
 804855c:	test   %esi,%esi
 804855e:	je     8048585 <__libc_csu_init+0x55>
 8048560:	xor    %edi,%edi
 8048562:	lea    0x0(%esi),%esi
 8048568:	sub    $0x4,%esp
 804856b:	pushl  0x2c(%esp)
 804856f:	pushl  0x2c(%esp)
 8048573:	push   %ebp
 8048574:	call   *-0xf8(%ebx,%edi,4)
 804857b:	add    $0x1,%edi
 804857e:	add    $0x10,%esp
 8048581:	cmp    %esi,%edi
 8048583:	jne    8048568 <__libc_csu_init+0x38>
 8048585:	add    $0xc,%esp
 8048588:	pop    %ebx
 8048589:	pop    %esi
 804858a:	pop    %edi
 804858b:	pop    %ebp
 804858c:	ret    
 804858d:	lea    0x0(%esi),%esi

08048590 <__libc_csu_fini>:
 8048590:	repz ret 

Disassembly of section .fini:

08048594 <_fini>:
 8048594:	push   %ebx
 8048595:	sub    $0x8,%esp
 8048598:	call   8048370 <__x86.get_pc_thunk.bx>
 804859d:	add    $0x1a63,%ebx
 80485a3:	add    $0x8,%esp
 80485a6:	pop    %ebx
 80485a7:	ret  

```

## 设置变量的值：movl指令

建议结合[《x86汇编语言语法简介》](http://www.xumenger.com/x86-20160720/)、[cpu-register](https://github.com/xumenger/xumenger.github.crack/tree/master/20170313-8086-cpu-register)阅读理解，汇编有Intel和AT&T两种风格，其源操作数和目的操作数是相反的，这点需特别注意

`movl   $0xabcd,-0x24(%ebp)`作用是将0xabcd的值赋给-0x24(%ebp)，ebp是基数指针寄存器，-0x24(%ebp)表示从ebp寄存器中取出的地址值减去0x24后的地址值，然后寻址，将0xabcd写到这个地址处

## 用if语句比较变量：cmpl指令

`cmpl   $0x1234,-0x24(%ebp)`比较0x1234和-0x24(%ebp)（也就是变量i），接下来如果i=0x1234，就不执行i=0，用`je     80484d7 <main+0x63>`跳转

cmpl指令比较变量i和0x1234，如果为真，则设置CPU的ZF寄存器为1，je指令在ZF标志位为1时执行跳转

## while语句的汇编代码

`cmpl   $0x0,-0x24(%ebp)`指令判断while语句的条件表达式，如果i==0，则用`je     80484d3 <main+0x5f>`跳转到80484d3地址处，执行`addl   $0x1,-0x24(%ebp)`（也就是80484d3地址处的指令）也就是i++语句

如果i!=0，就不执行je跳转，直接跳过je指令，执行下一条`call   8048445 <func>`

## 函数调用：call指令

call指令可以跳转到函数后再返回。从(6)`call   8048445 <func>`可以看出控制权转移到了func()函数

func()函数执行结束后，返回mian()

## 函数指针调用

调用函数指针func\_op()的代码在(9)，函数指针保存在eax中，要像`*%eax`这样加上*号

## 数组操作：movzbl指令

数组操作

## 返回值设置

(12)为return语句，给返回值赋0。return语句的返回值为int等4字节以下的情况，则要将返回值放到eax中，是以eax寄存器作为通用寄存器的
