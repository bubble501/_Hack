#include<stdio.h>
#include<string.h>

//有两种回文串："abccba"和"abcba"
//比较特殊的："aa"和"aaa"
char* longestPalindrome(char* s){
    int position = 0;
    int distanceBefore = 0;
    int distanceAfter = 0;
    int length = strlen(s);
    for(int i=0; i<length; i++){
        int min = i;
        if(length - i - 1 > i){
            min = length - i - 1;
        }
        for(int j=1; j<=min; j++){
            int jDistanceBefore = 0;
            int jDistanceAfter = 0;
            if((s[i] != s[i+1]) && (s[] != s[j+1])){
                
            }
        }
    } 
   return "test";
}

int main(){
    char* testStr = "abcdeffedcbaxxhh";
    printf("最大回文子串1：%s\n", longestPalindrome(testStr));

    testStr = "aaaaaaa";
    printf("最大回文子串2：%s\n", longestPalindrome(testStr));

    printf("\n");
    return 0;
}
