#include<stdio.h>
#include<string.h>

int lengthOfLongestSubstring(char* s){
    int length = 0;
    int tmp = 0;
    int out = 0;
    int i, j, k;
    //最外层循环为了分别找到从0、1、2...开始的最长不重复子串
    for(i=0; i<strlen(s); i++){
        tmp = 1;
        //这层是找出第i位开始最长的子串到哪位结束
        for(j=i+1; j<strlen(s); j++){
            //从i到j保证没有重复的字符
            for(k=i; k<j; k++){
                //如果发现在i到j之间有重复字符
                if(s[j] == s[k]){
                    out = 1;
                    break;
                }
            }
            //如果发现i和j之间有重复字符，那么就得到了i开始最长的子串
            if(1 == out){
                out = 0;
                tmp = j - i;
                break;
            }
            //如果没有重复字符，相当于循环到了最后
            tmp = j - i + 1;
        }
        if(tmp > length){
            length = tmp;
        }
    }
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
