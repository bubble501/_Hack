#include <stdio.h>
#include "cList.h"

/*
 * 初始化链表
 */
CList *clist_init(void (*destroy)(void *data))
{
    CList *list = (CList *)malloc(sizeof(CList));
    list->size = 0;
    list->destroy = destroy;
    list->head = NULL;
    list->tail = NULL;
    return list;
}

/*
 * 释放链表资源
 */
void clist_destroy(CList *list)
{
    void *data;
    while(clist_size(list) > 0){
        if(clist_rem_next(list, NULL, (void **)&data) == 0 && list->destroy != NULL){
            list->destroy(data);
        }
    }
    memset(list, 0, sizeof(CList));
    free(list);
    return;
}

/*
 * 在list的element后插入一个元素
 */
int clist_ins_next(CList *list, ListElmt *element, const void *data)
{
    ListElmt *new_element;
    if((new_element = (ListElmt *)malloc(sizeof(ListElmt))) == NULL){
        return -1;
    }

    new_element->data = (void *)data;
    //如果element为NULL，新元素放到链表头
    if(NULL == element){
        if(0 == clist_size(list)){
            list->tail = new_element;
        }
        new_element->next = list->head;
        list->head = new_element;
    }
    else{
        if(NULL == element->next){
            list->tail = new_element;
            new_element->next = NULL;
        }
        new_element->next = element->next;
        element->next = new_element;
    }

    list->size++;
    return 0;
}

/*
 * 移除由list指定的链表中element后面的元素
 */
int clist_rem_next(CList *list, ListElmt *element, void **data)
{
    if(0 == list->size){
        return -1;
    }
    ListElmt *old_element;
    //如果element为NULL，则移除头元素
    if(NULL == element){
        *data = list->head->data;
        old_element = list->head;
        list->head = list->head->next;
        if(1 == clist_size(list)){
            list->tail = NULL;
        }
    }
    else{
        if(NULL == element->next){
            return -1;
        }
        *data = element->next->data;
        old_element = element->next;
        element->next = element->next->next;
        if(NULL == element->next){
            list->tail = element;
        }
    }

    free(old_element);
    list->size --;
    return 0;
}
