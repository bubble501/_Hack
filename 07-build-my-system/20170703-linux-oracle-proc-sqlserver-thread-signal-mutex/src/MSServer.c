#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include </usr/local/freetds/include/sybfront.h>
#include </usr/local/freetds/include/sybdb.h>
#include "MSServer.h"

//insert into oiw03..ashare_ordwth (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (1, '20170702', '15:22:29', '0000235350', '5432154321', '600000', 'B', 10.0, 100, 'P', 'ORD', 0);
char *formatSQL = " insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d) ";
char *formatSQL_10 = " insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d);\
                       insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d);\
                       insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d);\
                       insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d);\
                       insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d);\
                       insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d);\
                       insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d);\
                       insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d);\
                       insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d);\
                       insert into %s (rec_num, date, time, reff, acc, stock, bs, price, qty, status, owflag, ordrec) values (%d, '%s', '%s', '%s', '%s', '%s', '%s', %.2f, %d, '%s', '%s', %d); ";
//作为全局变量在这里做测试可以，但是不能在实际中如此随意的使用
DBPROCESS* dbprocess = NULL;
char SQL[20000] = {0};
int rec_num = 1;                //rec_num从1开始，要求每次测试时将SQL Server清库

int MSServerInit(char* sDbUser, char* sDbPwd, char* sDbServer)
{
    char szUserName[32] = "";
    char szPassword[32] = "";
    char szServer[32] = "";
    strcpy(szUserName, sDbUser);
    strcpy(szPassword, sDbPwd);
    strcpy(szServer, sDbServer);

    dbinit();
    LOGINREC* loginrec = dblogin();
    DBSETLUSER(loginrec, szUserName);
    DBSETLPWD(loginrec, szPassword);
    dbprocess = dbopen(loginrec, szServer);
    if(FAIL == dbprocess){
        return -1;
    }
    return 0;
}

int SendEntrust(EntrustData *aEntrust, char *sTableName)
{
    if (NULL == aEntrust){
        return -1;
    }

    /*每次insert 一条记录
    //rec_num, date, time, reff, acc,    stock, bs,     price, qty, status, owflag, ordrec
    //%d,            '%s', '%s', '%s', '%s', '%s',    '%s', %f,        %d,    '%s',     '%s',     %d
    int ret = sprintf(SQL, formatSQL, sTableName, rec_num, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0);
    rec_num++;
    dbcmd(dbprocess, SQL);
    if(dbsqlexec(dbprocess) == FAIL){
        printf("委托序号%d\n", aEntrust->l_entrust_serial_no);
        printf("SQL = %s\n", SQL);
        return -1;
    }
    */

    //每次模拟10条进行insert
    int ret = sprintf(SQL, formatSQL_10, sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0,\
                                         sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0,\
                                         sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0,\
                                         sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0,\
                                         sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0,\
                                         sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0,\
                                         sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0,\
                                         sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0,\
                                         sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0,\
                                         sTableName, rec_num++, "20170702", "15:22:29", "0000235350", "5432154321", "600000", "B", 10.0, 100, "P", "ORD", 0);
    dbcmd(dbprocess, SQL);
    if(dbsqlexec(dbprocess) == FAIL){
        printf("委托序号%d\n", aEntrust->l_entrust_serial_no);
        printf("SQL = %s\n", SQL);
        return -1;
    }

    return 0;
}
