#include <stdio.h>
#include <pthread.h>
#include "../cQueue/cQueue.h"
#include "cLockQueue.h"

CLockQueue *clockqueue_init(void (*destroy)(void *data))
{
    CLockQueue *lockqueue = (CLockQueue *)malloc(sizeof(CLockQueue));
    pthread_mutex_init(&(lockqueue->lock), NULL);
    lockqueue->queue = cqueue_init(destroy);
    return lockqueue;
}

void clockqueue_destroy(CLockQueue *lockqueue)
{
    pthread_mutex_destroy(&(lockqueue->lock));
    cqueue_destroy(lockqueue->queue);
    free(lockqueue);
}

/*
 * 入队元素放入队尾
 */
int clockqueue_enqueue(CLockQueue *lockqueue, const void *data)
{
    pthread_mutex_lock(&lockqueue->lock);
    int ret = cqueue_enqueue(lockqueue->queue, data);
    pthread_mutex_unlock(&lockqueue->lock);
    return ret;
}

/*
 * 出队：从队头取出元素
 */
int clockqueue_dequeue(CLockQueue *lockqueue, void **data)
{
    pthread_mutex_lock(&lockqueue->lock);
    int ret = cqueue_dequeue(lockqueue->queue, data);
    pthread_mutex_unlock(&lockqueue->lock);
    return ret;
}
