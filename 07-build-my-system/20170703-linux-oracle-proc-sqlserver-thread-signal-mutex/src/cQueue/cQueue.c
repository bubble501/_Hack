#include "../cList/cList.h"
#include "cQueue.h"

/*
 * 入队元素放入队尾
 */
int cqueue_enqueue(CQueue *queue, const void *data)
{
    return clist_ins_next(queue, clist_tail(queue), data);
}

/*
 * 出队：从队头取出元素
 */
int cqueue_dequeue(CQueue *queue, void **data)
{
    int ret = clist_rem_next(queue, NULL, data);
    if(0 != ret){
        *data = NULL;
    }
    return ret;
}
