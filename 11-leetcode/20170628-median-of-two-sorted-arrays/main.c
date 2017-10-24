#include<stdio.h>

//数组是从0开始索引的，不是从1开始索引的，靠，这个常识竟然忘了
double findMedianSortedArray(int* nums, int numsSize){
    if ((NULL == nums) || (0 == numsSize)){
        return 0.0;
    }
    if(numsSize % 2 == 0){
        int flag = numsSize / 2;
        //如果有偶数个，并不返回真正的中位数，比如1、2、3、4，返回2
        return nums[flag - 1];
    }
    else{
        int flag = numsSize / 2;
        //如果有奇数个，则返回最中间的数
        return nums[flag];
    }
}

double findMedianSortedArrays(int* nums1, int nums1Size, int* nums2, int nums2Size){
    if (((NULL == nums1)  && (NULL == nums2)) 
       || ((0 == nums1Size) && (0 == nums2Size))){
        return 0.0;
    }
    return 0.0;
}

int compare(double expect, double real, char* s){
    if(expect - real < 0.0001){
        printf("%s，比较通过！预期值 = 实际值：%f\n", s, expect);
        return 0;
    }
    else{
        printf("%s，比较失败！预期值：%f；实际值：%f\n", s, expect, real);
        return -1;
    }
}

//如果进一步优化，可以将测试用例（两个数组、一个预期输出）按一定格式维护在文件中
//程序只是加载数据，然后比较，这样就不需要修改程而方便的测试各种数据了
//这样就更加符合自动化测试的思维了
int main(){
    int testNums1_1[2] = {1, 3};
    int testNums2_1[1] = {2};
    double real_1 = findMedianSortedArrays(testNums1_1, 2, testNums2_1, 1);
    compare(2, real_1, "测试用例1");

    int testNums1_2[9] = {0, 2, 4, 6, 8, 10, 20, 100, 1000};
    int testNums2_2[5] = {1, 3, 5, 7, 9};
    double real_2 = findMedianSortedArrays(testNums1_2, 9, testNums2_2, 5);
    compare(6.5, real_2, "测试用例2");

    printf("\n");
    return 0;
}
