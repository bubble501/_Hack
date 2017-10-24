[《如何理解并正确使用MySql索引》](https://my.oschina.net/feinik/blog/1305784)

---

# 概述

索引是存储引擎用于快速查找记录的一种数据结构，通过合理的使用数据库索引可以大大提升系统的访问性能，接下来主要介绍在MySQL数据库中索引类型，以及如何创建出更加合理且高效的索引技巧

>注：这里主要针对的是InnoDB存储引擎和D+Tree索引数据结构

# 索引的优点

1. 大大减轻了服务器需要扫描的数据量，从而提升了数据的检索速度
2. 帮助服务器避免排序和临时表
3. 可以将随机IO变为顺序IO

# 索引的创建

### 主键索引

```
ALTER TABLE 'table_name' ADD PRIMARY KEY 'index_name' ('column')
```

### 唯一索引

```
ALTER TABLE 'table_name' ADD UNIQUE 'index_name' ('column');
```

### 普通索引

```
ALTER TABLE 'table_name' ADD INDEX 'index_name' ('column');
```

### 全文索引

```
ALTER TABLE 'table_name' ADD FULLTEXT 'index_name' ('column');
```

### 组合索引

```
ALTER TABLE 'table_name' ADD INDEX 'index_name' ('column1', 'column2', ...);
```

# B+Tree的索引规则

创建一个测试的用户表

```
DROP TABLE IF EXISTS user_test;
CREATE TABLE user_test(
        id int AUTO_INCREMENT PRIMARY KEY, 
        user_name varchar(30) NOT NULL,
        sex bit(1) NOT NULL DEFAULT b'1',
        city varchar(50) NOT NULL,
        age int not NULL
)ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

创建一个组合索引

```
ALTER TABLE user_test ADD INDEX idx_user(username, city, age);
```

### 索引有效的查询

**全值匹配**

全值匹配指的是和索引中的所有列进行匹配，比如以上面创建的索引为例，在where条件后可以同时查询(user_name, city, age)为条件的数据

>注：与where后查询条件的顺序无关，这里是很多同学容易误解的一个地方

```
SELECT * RFOM user_test WHERE user_name='xumenger' and age=24 and city='徐州';
```

**匹配最左前缀**

匹配最左前缀是指有限匹配最左索引列，比如上面创建的索引可用于查询条件为：(user_name)、(user_name, city)、(user_name, city, age)

>注：满足最左前缀查询条件的顺序与索引列的顺序无关，如(city, user_name)、(age, city, user_name)

**匹配列前缀**

指匹配列值的开头部分，比如查询用户名以feinik开头的所有用户

```
SELECT * FROM user_test WHERE user_name LIKE 'feinik%';
```

**匹配范围值**

比如查询以用户名feinik开头的所有用户，这里使用了索引的第一列

```
SELECT * FROM user_test WHERE user_name LIKE 'feinik%';
```

### 索引的限制

where查询条件中不包含索引列中的最左索引列，则无法使用到索引查询，如

```
SELECT * FROM user_test WHERE city='徐州';
```

或

```
SELECT * FROM user_test WHERE age=26;
```

或

```
SELECT * FROM user_test WHERE city='徐州' AND age=26;
```

即使where的查询条件为最左索引列，也无法使用索引查询用户名以feinik结尾的用户

```
SELECT * FROM user_test WHERE user_name like '%feinik';
```

如果where查询条件中有某个列的范围查询，则其最右边的所有列都无法使用索引优化查询，如

```
SELECT * FROM user_test WHERE user_name='feinik' AND city LIKE '广州%' AND age=26
```

# 高效的索引策略

索引列不能是表达式的一部分，也不能作为函数的参数，否则无法使用索引查询

```
SELECT * FROM user_test WHERE user_name=concat(user_name, 'fei');
```

### 前缀索引

有时候需要索引很长的字符列，这会增加索引的存储空间以及降低索引的效率，一种策略是可以使用哈希索引，还有一种就是可以使用前缀索引，前缀索引是选择字符列的前n个字符作为索引，这样可以大大节约索引空间，从而提高索引效率

**前缀索引的选择性**

(1)

前缀索引要选择足够长的前缀以保证高的选择性，同时又不能太长，我们可以使用以下方式来计算出合适的前缀索引的**选择长度值**

```
SELECT COUNT(DISTINCT index_column)/COUNT(*) FROM table_name;
--index_column代表要添加前缀索引的列
```

>注：通过以上方式来计算出前缀索引的选择性比值，比值越高说明索引的效率也就越高

(2)

```
SELECT
    COUNT(DISTINCT LEFT(index_column, 1))/COUNT(*),
    COUNT(DISTINCT LEFT(index_column, 2))/COUNT(*),
    COUNT(DISTINCT LEFT(index_column, 3))/COUNT(*),
    ...
FROM table_name;
```

>注：通过上面语句逐步找到最接近与(1)中的前缀索引的选择性比值，那么就可以使用对应的字符截取长度来做前缀索引了

**前缀索引的创建**

```
ALTER TABLE table_name ADD INDEX index_name(index_column(length));
```

**使用前缀索引的注意点**

前缀索引是一种能使索引更小、更快的有效方法，但是MySQL无法使用前缀索引做ORDER BY和GROUP BY以及使用前缀索引做覆盖扫描

### 选择合适的索引列顺序

在组合索引的创建中索引列的顺序非常重要，正确的索引顺序依赖于使用该索引的查询方式，对于索引组合的索引顺序可以通过经验法则来帮助我们完成：

>将选择性最高的列放到索引最前列，该法则与前缀索引的选择性方法一致，但并不是说所有的组合索引的顺序都使用该法则就能确定，还需要根据具体的查询场景来确定具体的索引顺序

### 聚集索引与非聚集索引

**聚集索引**

聚集索引决定数据在物理磁盘上的物理排序，一个表只能有一个聚集索引，如果定义了主键，那么InnoDB会通过主键来聚集数据，如果没有定义主键，InnoDB会选择一个唯一的非空索引代替，如果没有唯一的非空索引，InnoDB会隐式定义一个主键来作为聚集索引

聚集索引可以很大程度的提高访问速度，因为聚集索引将索引和行数据保存在了同一个B-Tree中，所以找到了索引也就相应的找到了对应的行数据，但在使用聚集索引的时候需要注意避免随机的聚集索引（一般指主键值不连续，且分布范围不均匀），如使用UUID来作为聚集索引性能会很差，因为UUID的不连续会导致增加很多的索引碎片和随机IO，最终导致查询的性能急剧下降

**非聚集索引**

与聚集索引不同的是非聚集索引并不决定数据在磁盘上的物理顺序，且在B-Tree中包含索引但不包含行数据，行数据只是通过保存在B-Tree中的索引对应的指针来指向行数据，如：上面在(user_name, city, age)上建立的索引就是非聚集索引

### 覆盖索引

如果一个索引（如组合索引）中包含所有要查询的字段的值，那么就称之为覆盖索引，比如

```
SELECT user_name, city, age FROM user_test WHERE user_name='feinik' AND age >25;
```

因为要查询的字段(user_name, city, age)都包含在组合索引的索引列中，所以就使用了覆盖索引查询，查看是否使用了覆盖索引可以通过执行计划中的Extra中的值Using index则证明使用了覆盖索引，覆盖索引可以极大的提升访问性能

### 如何使用索引来排序

在排序操作中如果能使用到索引来排序，那么可以极大的提高排序的速度，要使用索引来排序需要满足以下两点即可：

* ORDER BY子句后的列顺序要与组合索引的列顺序一致，且所有排序列的排序方向（正序/倒序）需要一致
* 所查询的字段值需要包含在索引列中，即满足覆盖索引

**通过例子来具体分析**

在user__test表上创建一个组合索引

```
ALTER TABLE user_test ADD INDEX index_user(user_name, city, age);
```

可以使用到索引排序的案例

```
1. SELECT user_name, city, age FROM user_test ORDER BY user_name;

2. SELECT user_name, city, age FROM user_test ORDER BY user_name, city;

3. SELECT user_name, city, age FROM user_test ORDER BY user_name DESC, city DESC;

4. SELECT user_name, city, age FROM user_test WHERE user_name='feinik' ORDER BY city;
```

>注：第四点比较特殊一点，如果where查询条件为索引的第一列，且为常量条件，那么也可以使用到索引

**无法使用索引排序的案例**

sex不在索引列中

```
SELECT user_name, city, age FROM user_test ORDER BY user_name, sex;
```

排序列的方向不一致

```
SELECT user_name, city, age FROM user_test ORDER BY user_name ASC, city DESC;
```

所要查询的字段sex没有包含在索引列中

```
SELECT user_name, city, age, sex FROM user_test ORDER BY user_name;
```

where查询条件后的user_name为范围查询，所以无法使用到索引的其他列

```
SELECT user_name, city, age FROM user_test WHERE user_name LIKE 'feinik%' ORDER BY city;
```

多表连接查询时，只有当ORDER BY后的排序字段都是第一个表的索引列（需要满足以上索引排序的两个规则）时，方可使用索引排序。如：再创建一个用户的扩展表user\_test\_ext，并建立uid的索引

```
DROP TABLE IF EXISTS user_test_ext;
CREATE TABLE user_test_ext(
       id int AUTO_INCREMENT PRIMARY KEY,
       uid int NOT NULL,
       u_password VARCHAR(64) NOT NULL
)ENGINE-InnoDB DEFAULT CHARSET=utf8;
```

下面的SQL走索引排序

```
SELECT user_name, city, age FROM user_test u LEFT JOIN user_test_ext ue ON u.id=ue.uid ORDER BY u.user_name;
```

下面的SQL不走索引排序

```
SELECT user_name, city, age FROM user_test u LEFT JOIN user_test_ext ue ON u.id=ue.uid ORDER BY ue.uid;
```

# 总结

本文主要讲了B+Tree树结构的索引规则，不同索引的创建，以及如何正确的创建出高效的索引技巧来尽可能的提高查询速度。当然了，关于索引的使用技巧不单单只有这些，关于索引的更多技巧还需要平时不断的积累相关经验！
