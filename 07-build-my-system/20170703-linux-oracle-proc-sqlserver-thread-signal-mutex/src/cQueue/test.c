#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "cQueue.h"

#define MAXCOUNT 100000

typedef struct TTestData{
     int I1;
     int I2;
}TestData;

int main()
{
     CQueue *queue = cqueue_init(NULL);
     TestData *test = NULL;

     struct timeval tv, tv1;
     gettimeofday(&tv, NULL);
     printf("开始循环Push\n");
     int count = MAXCOUNT;
     while(count > 0){
          test = (TestData *)malloc(sizeof(TestData)); 
          test->I1 = count;
          test->I2 = count;
          //printf("循环enqueue数据%d\n", count);
          if(0 != cqueue_enqueue(queue, (void *)test)){
               printf("循环Push数据失败\n");
               free(test);
               cqueue_destroy(queue);
               return -2;
          }
          count--;
     }
     gettimeofday(&tv1, NULL);
     long sendus = tv1.tv_sec * 1000000 + tv1.tv_usec - (tv.tv_sec * 1000000 + tv.tv_usec);
     printf("Push完成%d个，耗时%ldus\n", MAXCOUNT, sendus);

     printf("开始循环Pop\n");
     while(cqueue_size(queue) > 0){
          cqueue_dequeue(queue, (void **)(&test));
          //printf("循环dequeue结果%d\n", test->I1);
          free(test);
     }
     gettimeofday(&tv, NULL);
     sendus = tv.tv_sec * 1000000 + tv.tv_usec - (tv1.tv_sec * 1000000 + tv1.tv_usec);
     printf("Pop完成%d个，耗时%ldus\n", MAXCOUNT, sendus);

     cqueue_destroy(queue);
     return 0;
}