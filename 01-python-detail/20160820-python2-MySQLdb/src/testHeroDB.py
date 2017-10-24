#!/usr/bin/env python

import MySQLdb
from heroDB import HeroDB

def main():
    conn = MySQLdb.connect(host='localhost', user='root', passwd='root', db='hero', port=3306, charset='utf8')
    cur = conn.cursor()

    #-------------------------create------------------------
    hero = HeroDB('hero', conn, cur)
    
    print '-' * 60
    print 'create table'
    hero.createTable('heros')

    #-------------------------insert------------------------
    hero.insert('heros', [3, 'xumenger', 0, 200, 'perfect'])

    #-------------------------select------------------------
    print '-' * 60
    print 'first record'
    result = hero.selectFirst('heros')
    print result

    print '-' * 60
    print 'last record'
    result = hero.selectLast('heros')
    print result

    print '-' * 60
    print 'more record'
    results = hero.selectNRecord('heros', 3)
    for item in results:
        print item

    print '-' * 60
    print 'all record'
    results = hero.selectAll('heros')
    for item in results:
        print item

    #--------------------------update------------------------
    print '-' * 60
    print 'update single'
    hero.updateSingle('heros', ['joker', 1, 22000, 'joker', 2])

    print '-' * 60
    print 'update'
    values = []
    values.append(['zhangsan', 1, 1200, 'zhangsan', 1])
    values.append(['lisi', 2, 1300, 'lisi', 2])
    values.append(['wangwu', 1, 2000, 'wangwu', 3])
    hero.update('heros', values)

    #--------------------------delete-------------------------
    print '-' * 60
    print 'delete by id'
    hero.deleteByID('heros', 1)

    print '-' * 60
    print 'drop table'
    hero.dropTable('heros')

    # print '-' * 60
    # print 'drop db'
    # hero.dropDB('hero')

if __name__ == '__main__':
    main()
