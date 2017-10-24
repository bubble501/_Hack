#ifndef CLIST_H
#define CLIST_H

#include <stdlib.h>

//定义链表的单个元素结构体
typedef struct ListElmt_
{
    void *data;
    struct ListElmt_ *next;
}ListElmt;

//定义链表数据结构
typedef struct CList_
{
    int size;
    int (*match)(const void *key1, const void *key2);
    void (*destroy)(void *data);

    ListElmt *head;
    ListElmt *tail;
}CList;

//定义链表相关的接口
CList *clist_init(void (*destroy)(void *data));
void clist_destroy(CList *list);
int clist_ins_next(CList *list, ListElmt *element, const void *data);
int clist_rem_next(CList *list, ListElmt *element, void **data);

#define clist_size(list) ((list)->size)
#define clist_head(list) ((list)->head)
#define clist_tail(list) ((list)->tail)
#define clist_is_head(list, element) ((element) == (list)->head ? 1 : 0)
#define clist_is_tail(list, element) ((element) == (list)->tail ? 1 : 0)
#define clist_data(element) ((element)->data)
#define clist_next(element) ((element)->next)

#endif
