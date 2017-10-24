#include <stdio.h>
#include <stdlib.h>
#include "cStack.h"

typedef struct TTestData{
    int I1;
    int I2;
}TestData;

int main()
{
    CStack *stack = cstack_init(NULL);
    TestData *test = NULL;

    int count = 10;
    while(count > 0){
      test = (TestData *)malloc(sizeof(TestData)); 
        test->I1 = count;
        test->I2 = count;
        printf("循环Push数据%d\n", count);
        if(0 != cstack_push(stack, (void *)test)){
            printf("循环Push数据失败\n");
            free(test);
            cstack_destroy(stack);
            return -2;
        }
        count--;
    }

    while(cstack_size(stack) > 0){
        cstack_pop(stack, (void **)(&test));
        printf("循环Pop结果%d\n", test->I1);
        free(test);
    }

    cstack_destroy(stack);
    return 0;
}