/* 冒泡排序的基本思想是：每次比较两个相邻的元素，如果它们的顺序错误就把它们交换过来
 * 
 */

#include <stdio.h>

int main()
{
  int a[100], i, j, t, n;
  
  printf("请输入要排序的数的个数(注意不要超过100个)：");
  scanf("%d", &n);

  printf("开始输入%d个数\n", n);
  for(i=1; i<=n; i++){
    scanf("%d", &a[i]);
  }

  printf("开始进行冒泡排序\n");
  for(i=1; i<=n-1; i++){//n个数排序，只用进行n-1趟
    for(j=1; j<=n-1; j++){
      if(a[j] < a[j+1]){
        t = a[j];
        a[j] = a[j+1];
        a[j+1] = t;
      }
    }
  }

  printf("输出排序后的结果！\n");
  for(i=1; i<=n; i++){
    printf("%d ", a[i]);
  }

  getchar();
  getchar();

  return 0;
}
