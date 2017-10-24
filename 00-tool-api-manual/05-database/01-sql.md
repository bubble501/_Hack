SQL是一门ANSI的标准计算机语言，用来访问和操作数据库系统。SQL语句用于取回和更新数据库中的数据。SQL可与数据库程序协同工作，比如MS Access、DB2、Informix、MS SQL Server、Oracle、Sybase、MySQL以及其他数据库系统

不幸的是，存在很多不同版本的SQL语言，但为了与ANSI标准相兼容，它们必须以相似的方法共同地来支持一些主要的关键词，比如：select、update、delete、insert、where等

除了SQL标准之外，大部分SQL数据库程序都拥有它们自己的私有扩展

# 增删改查

### Oracle数据库

```
--创建数据表
create table testTable
(
iCol number(16),		--整型
fCol number(16, 8),		--浮点型
sCol varchar(255)		--字符串
);

--插入记录
insert into testTable values(10, 10.0, 'testData1');
insert into testTable values(20, 20.0, 'testData1');

--查询记录
select * from testTable t where t.icol = 10;
select * from testTable t where t.icol = 20 and t.scol = 'testData2';

--修改记录
update testTable t set t.icol = 100 where t.icol = 20;

--删除记录
delete testTable t where t.icol = 100;

```

### SQL Server数据库

```
--创建数据库

--创建数据表
use[testDB]
create table testTable
(
iCol int,			--整型
fCol float,			--浮点型
sCol varchar(255)	--字符串
);

--插入记录
insert into testDB..testTable values(10, 10.0, 'testData1');
insert into testDB..testTable values(20, 20.0, 'testData1');

--查询记录
select * from testDB..testTable where icol = 10;
select * from testDB..testTable where icol = 20 and scol = 'testData2';

--修改记录
update testDB..testTable set icol = 100 where icol = 20;

--删除记录
delete testDB..testTable where icol = 100;

```

### MySQL数据库

>Mac环境上，可能要执行`mysql.server start`先启动MySQL服务器，然后执行`mysql -u root -p root`进入命令模式

>测试的MySQL的版本是：mysql  Ver 14.14 Distrib 5.7.17, for osx10.12 (x86_64) using  EditLine wrapper

```
#创建数据库
create database testDB;

#创建表
use testDB;
create table testTable
(
`iCol` int,	
`fCol` float,
`sCol` varchar(255)
);

#插入记录
insert into testTable values(10, 10.0, 'testData1');
insert into testTable values(20, 20.0, 'testData1');

#查询记录
select * from testTable where icol = 10;
select * from testTable where icol = 20 and scol = 'testData2';

#修改记录
update testTable set icol = 100 where icol = 20;

#删除记录
delete from testTable where icol = 100;

```

# 判断、循环逻辑

### Oracle数据库

```
declare
i number;
begin
i:=0;
for i in 1..10 loop
    insert into testTable select 1, 2.0, 'test' from dual;
    if i = 5 then
       insert into testTable values(3, 4.0, 'test2');  
    end if;
end loop;
end;
```

### SQL Server数据库

```
declare @i int
declare @f float
declare @s varchar(10)
set @i=0
while @i <= 10
begin
	set @s = right('0000000000' + cast(@i as varchar(10)), 10)
	set @f = @i
	insert into testDB..testTable values(@i, @f, @s)
	if 5 = @i 
	begin
		insert into testDB..testTable values(10, 100.0, 'test10')
	end;
	set @i = @i + 1
end;
```

### MySQL数据库

目前我在测试中发现，如果要执行循环，需要放在存储过程里，先创建一个文件test.sql，写入下面的创建存储过程的脚本（注意文件中只能用空格，不能用Tab！）

```
delimiter $$
drop procedure if exists testProc;
create procedure testProc()
begin
  declare i int;
  set i = 1;
  while i < 11 do
    insert into testTable values (i, 10.0, 'test');
    if i = 5 then
      insert into testTable values (55, 55.0, 'test55');
    end if;
    set i = i + 1;
  end while;
end;
$$

delimiter ;
```

在mysql运行环境下输入形如`source /Users/xumenger/Desktop/code/Laboratory/test.sql`创建存储过程

然后在mysql命令行下执行`call testProc;`即可调用存储过程！然后就可以看到testTable中已经有这个存储过程写入的数据了

# SQL背后的原理

上面整理的这些SQL都是一些简单的语法点，没啥意思。而且这些写法也只是功能上能用，在规范性上还不行；另外像SQL的高级用法比如order by、group by等语法都没有涉及到

### 1、应用程序把查询SQL语句发给服务器

在数据层执行SQL语句时，应用程序会连接到相应的数据库服务器，把SQL语句发送给服务器处理

### 2、服务器解析请求的SQL语句

经常用查询分析器的朋友大概都知道这样一个事实，往往一个查询语句在第一次运行的时候需要执行特别长的时间，但如果你马上或者在一定时间内运行同样的语句，会在很短的时间内返回查询结果。原因如下：

* 服务器在接收到查询请求后，并不是马上去数据库查询，而是在数据库的计划缓存中找是否有相对应的执行计划，如果存在，就直接调用已经编译好的执行计划，节省了执行计划的编译时间（所以这就是为什么要使用绑定变量法的原因）
* 如果所查询的行已经存在于数据库缓冲存储区中，就不用查询物理文件了，而是从缓存中取数据，这样从内存中取数据就会比从硬盘上读取数据快得多，提高了查询效率

如果在SQL计划缓存中没有对应的执行计划，服务器首先会对用户请求的SQL进行语法校验，如果有语法错误，服务器会结束查询操作，并返回相应的错误信息给调用它的程序。这里返回的错误信息中，只会包含基本的语法错误信息，比如select写成selec等，如果是SQL中包含一个没有的列，在此步骤是检查不出来的，这一步是语法验证，语义是否正确放在下一步进行

语法符合后，就开始校验它的语义是否正确，例如表名、列名、存储过程等等数据库对象是否真正存在，如果发现有不存在的，就会报错给应用程序，同时结束查询

接下来就是获得对象的解析锁，我们在查询一个表时，首先服务器会对这个对象加锁，这是为了保证数据的统一性。如果不加锁，此时有数据插入，但因为没有加锁的原因，查询已经将这条记录读入，而有的插入会因为事务的失败回滚，就会形成脏读的现象

然后就是对数据库用户权限的验证，SQL语句语法、语义都正确，此时并不一定能够得到查询结果，如果数据库用户没有相应的访问权限，服务器会报出权限不足的错误给应用程序，在稍大的项目中，往往一个项目里面会包含好多个数据库连接串，这些数据库用户具有不同的权限，有的是只读权限，有的是只写权限，有的是可读可写，根据不同的操作选取不同的用户来执行！

解析的最后一步，就是确定最终的执行计划。当语法、语义、权限都验证通过后，服务器并不会马上给你返回结果，而是会针对你的SQL进行优化，选择不同的查询算法以最高效的形式返回给应用程序。例如做表联合查询时，服务器会根据开销成本来最终决定采用has join，merge join，还是loop join，采用哪一种索引会更高效等等，不过它的自动化优化是有限的，要想写出高效的查询SQL还是要写SQL的人来根据表结构、索引列、关联表的数据量等实际情况来进行优化

当确定好执行计划之后，就会把这个执行计划保存到SQL计划缓存中，下次再有相同的执行请求时，就直接从计划缓存中取，避免重新编译执行计划

### 3、语句执行

服务器对SQL语句解析完成后，服务器才会知道这条语句到底表达了什么意思，接下来才会真正的执行SQL语句。此时分两种情况

* 如果查询语句所包含的数据行已经读取到数据缓冲存储区的话，服务器会直接从数据缓冲存储区中读取数据返回给应用程序，避免了从物理文件中读取，提高了查询速度
* 如果数据行没有在数据缓冲存储区中，则会从物理文件中读取记录返回给应用程序，同时把数据行写入数据缓冲存储区种，供下次使用

>SQL缓存分好几种，这里有兴趣的话可以去搜索一下。有时因为缓存的存在，使得我们很难马上看出优化的结果，因为第二次执行有缓存的存在，所以会特别快，所以一般都是先消除缓存，然后比较优化前后的性能表现

下面是SQL Server常用的清空缓存的方法

```
DBCC DROPCLEANBUFFERS  --从缓冲池中删除所有清除缓冲区
DBCC FREEPROCCACHE     --从过程缓存中删除所有元素
DBCC FREESYSTEMCACHE   --从所有缓存中释放所有未使用的缓存条目
```

SQL Server 2005数据库引擎会事先在后台清理未使用的缓存条目，以使内存可用于当前条目。但是使用此命令从所有缓存中手动删除未使用的条目。这只能基本消除SQL缓存的影响，目前好像没有完全消除缓存的方案

### 补充：SQL执行顺序

* FROM子句返回初始结果集
* WHERE子句排除不满足搜索条件的行
* GROUP BY子句将选定的行收集到GROUP BY子句中各个唯一值的组中
* 选择列表中指定的聚合函数可以计算各组的汇总值
* 此外，HAVING子句排除不满足搜索条件的行
* 计算所有的表达式
* 使用ORDER BY对结果集进行排序
* 查找要搜索的字段

# 参考文章

* [《W3school SQL 教程》](http://www.w3school.com.cn/sql/index.asp)
* [《理解SQL原理，写出高效的SQL语句》](http://www.nowamagic.net/librarys/veda/detail/1502)
* [《sql语句执行原理》](http://blog.csdn.net/sscsgss/article/details/5659113)
* [《写SQL要学会使用"执行计划"》](http://blog.csdn.net/wangpeng047/article/details/12849331)
* [《SQL Server执行计划的理解》](http://www.cnblogs.com/kissdodog/p/3160560.html)
* [《简介如何查看执行计划以及执行计划的准确性》](http://www.cnblogs.com/wingsless/archive/2012/02/24/2367176.html)
* [《看懂SqlServer查询计划》](http://www.cnblogs.com/fish-li/archive/2011/06/06/2073626.html)
* [《Oracle性能优化-读懂执行计划》](http://blog.csdn.net/lifetragedy/article/details/51320192)
* [《给PLSQL插上飞翔的翅膀-PLSQL优化》](http://blog.csdn.net/lifetragedy/article/details/51013069)
* [《How does a relational database work》](http://coding-geek.com/how-databases-work/)
* [《数据库的原理，一篇文章搞定（一）》](http://blog.csdn.net/zhangcanyan/article/details/51439012)
* [《数据库的原理，一篇文章搞定（二）》](http://blog.csdn.net/zhangcanyan/article/details/51439021)
* [《数据库的原理，一篇文章搞定（三）》](http://blog.csdn.net/zhangcanyan/article/details/51439034)
* [《带您理解SQLSERVER是如何执行一个查询的》](http://www.cnblogs.com/lyhabc/p/3367274.html)
* [《SQLSERVER独特的任务调度算法"SQLOS"》](http://www.cnblogs.com/lyhabc/archive/2012/10/17/2728724.html)
* [《SQL Server SQLOS 的任务调度[转]》](http://www.cnblogs.com/xugang/archive/2012/06/18/2553625.html)
* [《表格数据流协议TDS》](http://www.cnblogs.com/shanyou/archive/2009/11/14/1602957.html)
* [《TDS协议解析（转载）》](http://www.cnblogs.com/yylqinghao/archive/2010/03/16/1687551.html)

