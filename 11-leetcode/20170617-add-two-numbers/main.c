#include<stdio.h>

struct ListNode{
    int val;
    struct ListNode *next;
};

struct ListNode* addTwoNumbers(struct ListNode *l1, struct ListNode *l2){
    if((NULL==l1) || (NULL==l2)){
        return NULL;
    }
    struct ListNode* head;
    struct ListNode* node;
    struct ListNode* tmp;
    int result;
    int singleDigit = 0;
    int tensDigit = 0;

    head = malloc(sizeof(struct ListNode));
    result = l1->val + l2->val + tensDigit;
    tensDigit = result / 10;
    singleDigit = result % 10;
    head->val = singleDigit;
    head->next = NULL;
    node = head;

    l1 = l1->next;
    l2 = l2->next;
    while((NULL != l1) || (NULL != l2)){
        result = tensDigit;
        if(NULL != l1){
            result = result + l1->val;
            l1 = l1->next;
        }
        if(NULL != l2){
            result = result + l2->val;
            l2 = l2->next;
        }
        tensDigit = result / 10;
        singleDigit = result % 10;
    
        tmp = node;
        node = malloc(sizeof(struct ListNode));
        node->val = singleDigit;
        node->next = NULL;
        tmp->next = node;
    }
    if(tensDigit > 0){
        tmp = node;
        node = malloc(sizeof(struct ListNode));
        node->val = tensDigit;
        node->next = NULL;
        tmp->next = node;
    }
    return head;
}

struct ListNode* prepareData(int* data, int size){
    if((0==size) || (NULL==data)){
        return NULL;
    }

    struct ListNode* head;
    struct ListNode* node;
    struct ListNode* tmp;
    int i;

    head = malloc(sizeof(struct ListNode));
    head->val = data[0];
    head->next = NULL;
    node = head;
    //之前将0号元素准备为头元素，下面从1循环
    for(i=1; i<size; i++){
        tmp = node;
        node = malloc(sizeof(struct ListNode));
        node->val = data[i];
        node->next = NULL;
        tmp->next = node;
    }
    return head;
}

void freeList(struct ListNode* node){
    struct ListNode* tmp;
    while(NULL != node){
        tmp = node;
        node = node->next;
        free(node);
    }
}

void printList(struct ListNode* node){
    while(NULL != node){
        printf("%d\n", node->val);
        node = node->next;
    }
}

int main()
{
    //准备测试数据
    int data1[3] = {2, 4, 3};
    int data2[5] = {5, 6, 6, 4, 5};
    struct ListNode* l1 = prepareData(data1, 3);
    struct ListNode* l2 = prepareData(data2, 5);

    //计算和
    struct ListNode* ret = addTwoNumbers(l1, l2);

    //输出链表信息
    printList(ret);
    
    //释放申请的内存
    freeList(l1);
    freeList(l2);
    freeList(ret);

    return 0;
}

