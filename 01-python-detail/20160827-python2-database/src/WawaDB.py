# -*- coding:utf-8 -*-
import os
import time
import bisect
import itertools
from datetime import datetime
import logging
  
default_data_dir = './data/'          #数据库文件和索引文件存储的目录
default_write_buffer_size = 1024*10   #写需要的内存缓冲区大小
default_read_buffer_size = 1024*10    #读需要的内存缓冲区大小
default_index_interval = 1000         #每隔1000条记录在索引文件中添加一条索引记录
  
def ensure_data_dir():
    if not os.path.exists(default_data_dir):
        os.makedirs(default_data_dir)

# 初始化：创建本测试程序必须的文件路径
def init():
    ensure_data_dir()


#数据库索引类
class WawaIndex:
    def __init__(self, index_name):
        # 'a+'读写模式打开文件
        # open的第三个参数：0代表buffer关闭（只适用于二进制模式），1代表line buffer（只适用于文本模式），>1表示初始化的buffer大小
        self.fp_index = open(os.path.join(default_data_dir, index_name + '.index'), 'a+', 1)
        self.indexes, self.offsets, self.index_count = [], [], 0
        self.__load_index()
  
    def __update_index(self, key, offset):
        self.indexes.append(key)
        self.offsets.append(offset)
  
    def __load_index(self):
        self.fp_index.seek(0)
        for line in self.fp_index:
            try:
                # line.split()  按照空格对当前行进行分割，返回分割后的多个子串
                # line.split(';')   按照';'对当前行进行分割，返回分割后的多个子串
                key, offset  = line.split()
                self.__update_index(key, offset)
            except ValueError: # 索引如果没有flush的话，可能读到有半行的数据
                pass
  
    def append_index(self, key, offset):
        self.index_count += 1
        # 每1000条记录，往索引文件中添加一条索引：索引值 对应在数据库文件中的偏移 换行符
        if self.index_count % default_index_interval == 0:
            self.__update_index(key, offset)
            self.fp_index.write('%s %s %s' % (key, offset, os.linesep))
  
    def get_offsets(self, begin_key, end_key):
        left = bisect.bisect_left(self.indexes, str(begin_key))
        right = bisect.bisect_left(self.indexes, str(end_key))
        left, right = left - 1, right - 1
        if left < 0: 
            left = 0
        if right < 0: 
            right = 0
        if right > len(self.indexes) - 1: 
            right = len(self.indexes) - 1
        logging.debug('get_index_range:%s %s %s %s %s %s', self.indexes[0], self.indexes[-1], begin_key, end_key, left, right)
        return self.offsets[left], self.offsets[right]


#数据库类
class WawaDB:
    def __init__(self, db_name):
        self.db_name = db_name
        # open的第二个参数a，表示以追加方式打开文件
        # open的第三个参数：0代表buffer关闭（只适用于二进制模式），1代表line buffer（只适用于文本模式），>1表示初始化的buffer大小
        self.fp_data_for_append = open(os.path.join(default_data_dir, db_name + '.db'), 'a', default_write_buffer_size)
        self.index = WawaIndex(db_name)
  
    def __get_data_by_offsets(self, begin_key, end_key, begin_offset, end_offset):
        # open的第三个参数：0代表buffer关闭（只适用于二进制模式），1代表line buffer（只适用于文本模式），>1表示初始化的buffer大小
        fp_data = open(os.path.join(default_data_dir, self.db_name + '.db'), 'r', default_read_buffer_size)
        fp_data.seek(int(begin_offset))

        # 读取文件中一行内容
        line = fp_data.readline()
        find_real_begin_offerset = False
        will_read_len, read_len = int(end_offset) - int(begin_offset), 0
        while line:
            read_len += len(line)
            if(not find_real_begin_offerset) and (line < str(begin_key)):
                line = fp_data.readline()
                continue
            find_real_begin_offerset = True
            if(read_len >= will_read_len) and (line > str(end_key)):
                break
            yield line.rstrip('\r\n')
            line = fp_data.readline()

    def append_data(self, data, record_time = datetime.now()):
        def check_args():
            if not data:
                raise ValueError('data is null')
            if not isinstance(data, basestring):
                raise ValueError('data is not string')
            if data.find('\r') != -1 or data.find('\n') != -1:
                raise ValueError('data contains linesep')
        check_args()
        
        record_time = time.mktime(record_time.timetuple())
        # 数据库文件中每条记录的格式：时间戳 数据 换行符
        # os.linesep字符串给出当前平台使用的终止符，Windows用'\r\n'，Linux用'\n'，Mac用'\r'
        data = '%s %s %s' % (record_time, data, os.linesep)
        offset = self.fp_data_for_append.tell()
        # 先将数据内容写到数据库文件
        self.fp_data_for_append.write(data)
        # 然后将时间戳、在数据库文件中的偏移量记录到索引文件
        self.index.append_index(record_time, offset)

    # 搜索时间戳在begin_time到end_time之间的符合条件data_filter的数据
    def get_data(self, begin_time, end_time, data_filter=None):
        def check_args():
            if not(isinstance(begin_time, datetime) and isinstance(end_time, datetime)):
                raise ValueError('begin_time or end_time is not datetime')
        check_args()

        begin_time, end_time = time.mktime(begin_time.timetuple()), time.mktime(end_time.timetuple())
        # 根据时间戳的范围，返回对应记录在数据库中的偏移范围
        begin_offset, end_offset = self.index.get_offsets(begin_time, end_time)

        # 按照索引的范围在数据库文件中获取记录，然后根据data_filter过滤出符合条件的数据
        for data in self.__get_data_by_offsets(begin_time, end_time, begin_offset, end_offset):
            if data_filter:
                if data_filter(data):
                    yield data
            else:
                yield data

# 测试程序
def test():
    from datetime import datetime, timedelta
    import uuid, random
    logging.getLogger().setLevel(logging.NOTSET)

    def time_test(test_name):
        def inner(f):
            def inner2(*args, **kargs):
                start_time = datetime.now() # datetime.now()获取当前时间，print datetime.now()格式为：2016-08-27 17:24:43.234532
                result = f(*args, **kargs)
                print '%s take time: %s' % (test_name, (datetime.now() - start_time))
                return result
            return inner2
        return inner

    @time_test('gen_test_data')
    def gen_test_data(db):
        now = datetime.now()
        # datetime.now() - timedelta(hours=5)       根据当前时间获取5小时之前时间的时间格式
        # datetime.now() - timedelta(minutes=3)     根据当前时间获取3分钟之前时间的时间格式
        begin_time = now - timedelta(hours = 5) 
        while begin_time < now:
            print begin_time
            for i in range(10000):
                db.append_data(str(random.randint(1, 10000)) + ' ' + str(uuid.uuid1()), begin_time)
                begin_time += timedelta(minutes=1)

    @time_test('test_get_data')
    def test_get_data(db):
        begin_time = datetime.now() - timedelta(hours = 3)
        end_time = begin_time + timedelta(minutes=120)
        # find，当在字符串中找不到子串时返回-1
        results = list(db.get_data(begin_time, end_time, lambda x: x.find('1023') != -1))
        print 'test_get_data get %s results' % len(results)

    @time_test('get_db')
    def get_db():
        return WawaDB('test')

    if not os.path.exists('./data/test.db'):
        db = get_db()
        gen_test_data(db)
        #db.index.fp_index.flush()

    db = get_db()
    test_get_data(db)

init()

if __name__ == '__main__':
    test()
