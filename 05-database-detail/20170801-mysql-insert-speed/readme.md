[《MySQL批量SQL插入性能优化》](https://segmentfault.com/a/1190000008890065)

这里是以MySQL为例进行实现，其实在SQL Server、Oracle中也是类似的原理。不过为了严谨最好自己去测试一下

---

对于一些数据量较大的系统，数据库面临的问题除了查询效率地下，还有就是数据入库的时间长。特别是像报表系统，每天花费在数据导入上的时间可能长达几个小时或十几个小时之久。因此，优化数据库插入性能是很有意义的

## 一条SQL语句插入多条数据

常用的插入语句如

```
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`)
    VALUES ('0', 'userid_0', 'content_0', 0);
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`)
    VALUES ('0', 'userid_1', 'content_1', 1);
```

修改成

```
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`)
    VALUES ('0', 'userid_0', 'content_0', 0),
           ('1', 'userid_1', 'content_1', 1);
```

修改后的插入操作能够提高程序的插入效率。这里第二种SQL执行效率高的主要原因是合并后后台日志量（MySQL的binlog和innodb的事务日志）减少了，**降低日志刷盘的数据量和频率，从而提升效率**

通过合并SQL语句，同时也能减少SQL解析的次数，减少网络传输的IO

这里提供一些测试对比数据，分别是进行单条数据的导入与转化为一条SQL语句进行导入，分别测试100、1000、10000条数据记录（数据基础记录数是50W）

记录数  |  单条数据插入  |  合并数据
--------|----------------|------------
100     | 0m0.109s       |  0m0.009s
1000    | 0m1.432s       |  0m0.026s
10000   | 0m13.092s      |  0m0.755s

## 在事务中进行插入处理

把插入修改成

```
START TRANSACTION;
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`)
    VALUES('0', 'userid_0', 'content_0', 0);
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`)
    VALUES('1', 'userid_1', 'content_1', 1);
...
COMMIT;
```

使用事务可以提高数据的插入效率，这是因为进行一个INSERT操作时，MySQL内部会建立一个事务，在事务内才进行真正插入处理操作。通过使用事务可以减少创建事务的消耗，`所有插入都在执行后才进行提交操作`

这里也提供了测试对比，分别是不使用事务与使用事务在记录数为100、1000、10000的情况（数据基础记录数是50W）

记录数  |  单条数据插入  |  合并数据
100     | 0m0.109s       | 0m0.015s
1000    | 0m1.432s       | 0m0.085s
10000   | 0m13.093s      | 0m1.003s

## 数据有序插入

数据有序的插入是指插入记录在主键上是有序排列，例如datetime是记录的主键

```
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`) 
    VALUES ('1', 'userid_1', 'content_1', 1);
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`) 
    VALUES ('0', 'userid_0', 'content_0', 0);
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`) 
    VALUES ('2', 'userid_2', 'content_2',2);
```

修改为

```
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`) 
    VALUES ('0', 'userid_0', 'content_0', 0);
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`) 
    VALUES ('1', 'userid_1', 'content_1', 1);
INSERT INTO `insert_table` (`datetime`, `uid`, `content`, `type`) 
    VALUES ('2', 'userid_2', 'content_2',2);
```

**由于数据库插入时，需要维护索引数据，无序的记录会增大维护索引的成本**。我们可以参照InnoDB使用的B+Tree索引，如果每次插入记录都在索引的最后面，索引的定位效率很高，并且对索引调整较小；如果插入的记录在索引中间，需要B+Tree进行分裂合并等出来，会消耗比较多计算资源，并且插入记录的索引定位效率会下降，数据量较大时会有频繁的磁盘操作！

下面剔红随机数据与顺序数据的性能对比，分别是记录为100、1000、10000、100000、1000000（数据基础记录数为50W）

记录数  | 单条数据插入(随机) | 单条数据插(有序)
--------|--------------------|-------------------
100     | 0m0.137s           | 0m0.122s
1000    | 0m1.555s           | 0m1.277s
1W      | 0m13.118s          | 0m13.197s
10W     | 2m11.274s          | 2m10.554s
100W    | 21m51.522s         | 21m47.174s

从测试结果来看，该优化方法的性能有所提高，但是提高并不是很明显

## 性能综合测试

这里提供了同时使用上面三种方法进行INSERT效率优化的测试（数据基础记录数为50W）

记录数  | 单条数据插入(随机) | 合并数据+事务(随机) | 合并数据+事务(有序)
--------|--------------------|---------------------|---------------------
100     | 0m0.137s           | 0m0.024s            | 0m0.026s
1000    | 0m1.155s           | 0m0.058s            | 0m0.056s
1W      | 0m13.118s          | 0m0.288s            | 0m0.319s
10W     | 2m11.274s          | 0m4.546s            | 0m2.830s
100W    | 21m51.522s         | 0m38.851s           | 0m27.774s
1000W   | 224m54.935s        | 21m13.036s          | 4m19.593s
2000W   | --                 | 180m41.477s         | 8m44.697s

从测试结果可以看到，合并数据+事务的方法在较小数据量时，性能提高很明显，数据量较大时（1000W以上），性能会急剧下降，`这是由于此时数据量超过了InnoDB\_buffer的容量，每次定位索引涉及较多的磁盘读写操作，性能下降较快`。而使用`合并数据+事务+有序数据`的方式在数据量达到千万级以上表现依旧良好，在数据量较大时，有序数据索引定位较为方便，不需要频繁对磁盘进行读写操作，所以可以维持较高的性能

**注意事项**

* `SQL语句是有长度限制的`，在进行数据合并在同一SQL中务必不能超过SQL长度限制，通过max\_allowed\_packet配置可以修改，默认是1M，测试时修改为8M
* `事务需要控制大小`，事务太大可能会影响执行的效率。MySQL有innodb\_log\_buffer\_size配置项，超过这个值会把innodb的数据刷到磁盘中，这时，效率会有所下降。所以比较好的做法是，在数据达到这个值之前进行事务提交