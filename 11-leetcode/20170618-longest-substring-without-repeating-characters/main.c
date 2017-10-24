#include<stdio.h>
#include<string.h>

int lengthOfLongestSubstring(char* s){
    int length = 0;

    return length;
}

int main()
{
    char* s1 = "abcabcbb";
    char* s2 = "bbbbb";
    char* s3 = "pwwkew";
    char* s4 = "c";
    char* s5 = "abcdefg";

    int len1 = lengthOfLongestSubstring(s1);
    int len2 = lengthOfLongestSubstring(s2);
    int len3 = lengthOfLongestSubstring(s3);
    int len4 = lengthOfLongestSubstring(s4);
    int len5 = lengthOfLongestSubstring(s5);

    printf("len1=%d; len2=%d; len3=%d; len4=%d; len5=%d\n", len1, len2, len3, len4, len5);

    return 0;
}
