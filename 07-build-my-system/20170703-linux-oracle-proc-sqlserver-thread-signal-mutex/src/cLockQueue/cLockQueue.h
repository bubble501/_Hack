#ifndef CLOCKQUEUE_H
#define CLOCKQUEUE_H
#include <pthread.h>
#include "../cQueue/cQueue.h"

typedef struct CLockQueue_
{
    pthread_mutex_t lock;
    CQueue *queue;
}CLockQueue;

CLockQueue *clockqueue_init(void (*destroy)(void *data));
void clockqueue_destroy(CLockQueue *lockqueue);

int clockqueue_enqueue(CLockQueue *lockqueue, const void *data);
int clockqueue_dequeue(CLockQueue *lockqueue, void **data);

#define clockqueue_peek(lockqueue) ((lockqueue->queue)->head == NULL ? NULL : (queue)->head->data)
#define clockqueue_size(lockqueue) cqueue_size(lockqueue->queue)

#endif