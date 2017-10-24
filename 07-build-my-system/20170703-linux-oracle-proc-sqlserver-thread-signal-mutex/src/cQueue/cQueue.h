#ifndef CQUEUE_H
#define CQUEUE_H
#include "../cList/cList.h"

typedef CList CQueue;

#define cqueue_init clist_init
#define cqueue_destroy clist_destroy

int cqueue_enqueue(CQueue *queue, const void *data);
int cqueue_dequeue(CQueue *queue, void **data);

#define cqueue_peek(queue) ((queue)->head == NULL ? NULL : (queue)->head->data)
#define cqueue_size clist_size

#endif