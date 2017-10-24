单纯的为了完成而完成是没啥意思的，总是要把思考、查资料、分析的过程做个总结，如此才能有进步

这个问题很简单：给一个整型数组、给一个目标整数，找出数组满足相加等于这个目标整数的两个元素的下标，这道题目中不需考虑特殊情况，leetcode给的测试用例总是有一对元素满足这个条件的

最简单的思维，就是两层循环：

```
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
```

>分析一个算法的好坏，就是最简单的两个标准：时间复杂度和空间复杂度

对于上面的这个算法实现，因为要两层循环，其时间复杂度是O(N^2)，空间复杂度是O(1)，因为只需要申请额外的2 * sizeof(int)的空间

好了，到这里看起来很一目了然，似乎这个就是最标准、唯一的解决方案了

## 更好的解决方案

然而，还有更好的解决方案！对时间复杂度还能进行更优的处理！

```
public int[] twoSum(int[] nums, int target){
	Map<Integer, Integer> map = new HashMap<>();
	for(int i=0; i<nums.length; i++){
		map.put(nums[i], i);
	}
	for(int i=0; i<nums.length; i++){
		int complement = target - nums[i];
		if(map.containskey(complement) && map.get(complement)!=i){
			return new int[]{i, map.get(complement)};
		}
	}
	throw new IllegalArguementException("No two sum solution");
}
```

>虽然最后时间复杂度和空间复杂度都是O(N)，时间复杂度上确实有提升，但这里需要使用更大的内存空间，因为数组本身需要存储，另外哈希表本身也需要存储空间。这其实依然是一个空间换时间的典型

哈希表确实是在有性能需求的地方有很好的用处！
