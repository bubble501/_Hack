grep用于对文本进行按行的匹配

`ifconfig | grep "lo0"` 对ifconfig输出的文本内存进行处理找到所有含有"lo0"的行

`grep "printf" main.c` 从main.c中找出所有有printf的行

grep和正则表达式配合使用能发挥出更大的作用，关于正则表达式推荐[《正则表达式与文件格式化处理》](http://www.xumenger.com/regex-20160904/)

`grep "^char" main.c` 找到main.c中所有以char字符串开头的行

`grep "[0-9]" main.c` 找到main.c中所有带有数字的行
