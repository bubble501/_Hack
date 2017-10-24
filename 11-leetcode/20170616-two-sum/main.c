#include<stdio.h>

int* twoSum(int* nums, int numSize, int target){
    int i, j;
    int* ret = NULL;
    for(i=0; i<numSize; i++){
        for(j=i+1; j<numSize; j++){
            if(nums[i] + nums[j] == target){
                ret = malloc(2 * sizeof(int));
                ret[0] = i;
                ret[1] = j;
                return ret;
            }
        }
    }
    return NULL;
}

int main()
{
    int nums[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    int* ret = twoSum(nums, 10, 17);
    if(NULL != ret){
        printf("nums[%d] + nums[%d] = 17\n", ret[0], ret[1]);
        free(ret);
    }

    return 0;
}
