#!/usr/bin/env python

import MySQLdb

DATABASE_NAME = 'hero'

class HeroDB:
    # init class and create a database
    def __init__(self, name, conn, cur):
        self.name = name
        self.conn = conn
        self.cur = cur
        try:
            cur.execute('create database if not exists ' + name)
            conn.select_db(name)
            conn.commit()
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # create a table
    def createTable(self, name):
        try:
            ex = self.cur.execute
            if ex('show tables') == 0:
                ex('create table ' + name + ' (id int, name varchar(20), sex int, age int, info varchar(50))')
                self.conn.commit()
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s"  (e.args[0]. e.args[1])

    # insert single record
    def insert(self, name, value):
        try:
            self.cur.execute('insert into ' + name + ' values(%s, %s, %s, %s, %s)', value)
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # insert more records
    def insertMore(self, name, values):
        try:
            self.cur.executemany('insert into ' + name + ' values(%s, %s, %s, %s, %s)', values)
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # update single record from table
    # name: table name
    # values: waiting to update data
    def updateSingle(self, name, value):
        try:
            self.cur.execute('update ' + name + ' set name=%s, sex=%s, age=%s, info=%s where id=%s;', value)
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # update some records from table
    def update(self, name, values):
        try:
            self.cur.executemany('update ' + name + ' set name=%s, sex=%s, age=%s, info=%s where id =%s;', values)
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # get record count from db table
    def getCount(self, name):
        try:
            count = self.cur.execute('select * from ' + name)
            return count
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # select first record from database
    def selectFirst(self, name):
        try:
            self.cur.execute('select * from ' + name + ' ;')
            result = self.cur.fetchone()
            return result
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # select last record from database
    def selectLast(self, name):
        try:
            self.cur.execute('select * from ' + name + ' order by id desc;')
            result = self.cur.fetchone()
            return result
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # select next n records from database
    def selectNRecord(self, name, n):
        try:
            self.cur.execute('select * from ' + name + ';')
            results = self.cur.fetchmany(n)
            return results
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # select all records
    def selectAll(self, name):
        try:
            self.cur.execute('select * from ' + name + ';')
            self.cur.scroll(0, mode='absolute')
            results = self.cur.fetchall()
            return results
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # delete a record
    def deleteByID(self, name, id):
        try:
            self.cur.execute('delete from ' + name + ' where id=%s;' % id)
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # delete some record
    def deleteSome(self, name):
        pass

    # drop the table 
    def dropTable(self, name):
        try:
            self.cur.execute('drop table ' + name + ';')
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    # drop the database
    def dropDB(self, name):
        try:
            self.cur.execute('drop database ' + name + ';')
        except MySQLdb.Error, e:
            print "MySQL Error %d: %s" % (e.args[0], e.args[1])

    def __del__(self):
        if self.cur != None:
            self.cur.close()
        if self.conn != None:
            self.conn.close()
