#include <stdio.h>
#include "../cList/cList.h"
#include "cStack.h"

/*
 * 压栈：放到栈头
 */
int cstack_push(CStack *stack, const void *data)
{
    return clist_ins_next(stack, NULL, data);
}

/*
 * 出栈：出栈头元素
 */
int cstack_pop(CStack *stack, void **data)
{
    return clist_rem_next(stack, NULL, data);
}
