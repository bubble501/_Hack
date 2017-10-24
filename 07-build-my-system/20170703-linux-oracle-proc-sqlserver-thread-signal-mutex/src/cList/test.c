#include <stdio.h>
#include <stdlib.h>
#include "cList.h"

typedef struct TTestData{
    int I1;
    int I2;
}TestData;

int main()
{
    CList *list = clist_init(NULL);

    TestData* test = (TestData *)malloc(sizeof(TestData)); 
    test->I1 = 1;
    test->I2 = 1;
    printf("插入数据%d\n", 1);
    if(0 != clist_ins_next(list, NULL, (void *)test)){
        printf("插入文件头失败\n");
        free(test);
        clist_destroy(list);
        return -1;
    }

    int count = 10;
    while(count > 0){
        test = (TestData *)malloc(sizeof(TestData)); 
        test->I1 = count;
        test->I2 = count;
        printf("循环插入数据%d\n", count);
        if(0 != clist_ins_next(list, list->head, (void *)test)){
            printf("循环插入数据失败\n");
            free(test);
            clist_destroy(list);
            return -2;
        }
        count--;
    }

    while(clist_size(list) > 0){
        clist_rem_next(list, NULL, (void **)(&test));
        printf("移除结果%d\n", test->I1);
        free(test);
    }

    clist_destroy(list);

    return 0;
}
