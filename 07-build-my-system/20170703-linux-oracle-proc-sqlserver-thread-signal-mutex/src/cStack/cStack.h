#ifndef CSTACK_H
#define CSTACK_H
#include "../cList/cList.h"

typedef CList CStack;

#define cstack_init clist_init
#define cstack_destroy clist_destroy

int cstack_push(CStack *stack, const void *data);
int cstack_pop(CStack *stack, void **data);

#define cstack_peek(stack) ((stack)->head == NULL ? NULL : (stack)->head->data)
#define cstack_size clist_size

#endif
