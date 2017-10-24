/*
  Copyright (c) 2009 Dave Gamble
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*/

#ifndef cJSON__h
#define cJSON__h

//用于让C兼容C++！
#ifdef __cplusplus
extern "C"
{
#endif

/* cJSON Types: */
#define cJSON_False 0		//false值
#define cJSON_True 1		//true值
#define cJSON_NULL 2		//null值
#define cJSON_Number 3		//数值
#define cJSON_String 4		//字符串
#define cJSON_Array 5		//数组
#define cJSON_Object 6		//对象
	
#define cJSON_IsReference 256
#define cJSON_StringIsConst 512

/* The cJSON structure: */
//定义解析、组建JSON时用到的结构体
typedef struct cJSON {
	//同一级的元素使用双向链表存储
	struct cJSON *next,*prev;	/* next/prev allow you to walk array/object chains. Alternatively, use GetArraySize/GetArrayItem/GetObjectItem */
	//如果是一个object或array的话，child为第一个儿子的指针
	struct cJSON *child;		/* An array or object item will have a child pointer pointing to a chain of the items in the array/object. */

	//表明其是 false 或 true 或 null 或 数值 或 字符串 或 数组 或 对象 类型
	int type;					/* The type of the item, as above. */

	//如果是字符串类型，则这里是字符串的值
	char *valuestring;			/* The item's string, if type==cJSON_String */
	//如果是数值类型（整型），则这里是整型值
	int valueint;				/* The item's number, if type==cJSON_Number */
	//如果是数值类型（浮点型），则这里是浮点值
	double valuedouble;			/* The item's number, if type==cJSON_Number */

	//如果是对象的key-value元素的话，该字段为key值
	char *string;				/* The item's name string, if this item is the child of, or is in the list of subitems of an object. */
} cJSON;

//cJSON的内存管理使用了HOOK技术，主要是为了方便使用者自己定义内存管理函数，即用户自定义的malloc和free
//用函数指针封装malloc，free，方便用于处理，比如在申请后初始化，或者释放前进行一些处理等
//通过函数指针实现HOOK，看起来还是很好理解的！就是名词听上去比较玄乎！
typedef struct cJSON_Hooks {
      void *(*malloc_fn)(size_t sz);
      void (*free_fn)(void *ptr);
} cJSON_Hooks;

/* Supply malloc, realloc and free functions to cJSON */
//对cJSON提供的分配、再分配、释放内存初始化函数
extern void cJSON_InitHooks(cJSON_Hooks* hooks);


//以下几个函数是在解析JSON结构时用到的

/* Supply a block of JSON, and this returns a cJSON object you can interrogate. Call cJSON_Delete when finished. */
//解析函数，传入一个Json结构的字符串，cJson_Parse完成对其的解析，并返回根节点的指针
extern cJSON *cJSON_Parse(const char *value);

/* Render a cJSON entity to text for transfer/storage. Free the char* when finished. */
//输出JSON字符串函数，以item为根节点，输出其下元素组成的JSON结构的字符串，并且做了格式化处理（缩进、换行）
extern char  *cJSON_Print(cJSON *item);
/* Render a cJSON entity to text for transfer/storage without any formatting. Free the char* when finished. */
//输出JSON字符串函数，没有进行格式化
extern char  *cJSON_PrintUnformatted(cJSON *item);
/* Render a cJSON entity to text using a buffered strategy. prebuffer is a guess at the final size. guessing well reduces reallocation. fmt=0 gives unformatted, =1 gives formatted */
extern char *cJSON_PrintBuffered(cJSON *item,int prebuffer,int fmt);

/* Delete a cJSON entity and all subentities. */
//删除函数，释放节点c
extern void   cJSON_Delete(cJSON *c);

/* Returns the number of items in an array (or object). */
//返回数组array下元素的个数
extern int	  cJSON_GetArraySize(cJSON *array);
/* Retrieve item number "item" from array "array". Returns NULL if unsuccessful. */
//返回数组array的第item个元素
extern cJSON *cJSON_GetArrayItem(cJSON *array,int item);
/* Get item "string" from object. Case insensitive. */
//获取对象object下，键值是string的节点
extern cJSON *cJSON_GetObjectItem(cJSON *object,const char *string);

/* For analysing failed parses. This returns a pointer to the parse error. You'll probably need to look a few chars back to make sense of it. Defined when cJSON_Parse() returns 0. 0 when cJSON_Parse() succeeds. */
extern const char *cJSON_GetErrorPtr(void);
	
	
//以下几个函数是在组建JSON结构时用到的	

/* These calls create a cJSON item of the appropriate type. */
extern cJSON *cJSON_CreateNull(void);		//创建一个NULL值节点，并且返回该节点指针
extern cJSON *cJSON_CreateTrue(void);		//创建一个布尔类型节点，值是True，并返回该节点指针
extern cJSON *cJSON_CreateFalse(void);		//创建一个布尔类型节点，值是False，并返回该节点指针
extern cJSON *cJSON_CreateBool(int b);		//创建一个布尔类型节点，值是b
extern cJSON *cJSON_CreateNumber(double num);	//创建一个数值类型节点，值是num
extern cJSON *cJSON_CreateString(const char *string);	//创建一个字符串类型节点，值是string
extern cJSON *cJSON_CreateArray(void);		//创建一个Array类型节点，Array元素使用其他方法添加
extern cJSON *cJSON_CreateObject(void);		//创建一个Object类型节点，Object熟悉使用其他方法添加

/* These utilities create an Array of count items. */
//int ids[4]={116,943,234,38793};
//cJSON_AddItemToObject(img,"IDs", cJSON_CreateIntArray(ids,4));
extern cJSON *cJSON_CreateIntArray(const int *numbers,int count);
extern cJSON *cJSON_CreateFloatArray(const float *numbers,int count);
extern cJSON *cJSON_CreateDoubleArray(const double *numbers,int count);
extern cJSON *cJSON_CreateStringArray(const char **strings,int count);

/* Append item to the specified array/object. */
//往数组array中添加一个节点item
extern void cJSON_AddItemToArray(cJSON *array, cJSON *item);
//往对象object中添加一个键值对属性，键是string，值存储在item中
extern void	cJSON_AddItemToObject(cJSON *object,const char *string,cJSON *item);
extern void	cJSON_AddItemToObjectCS(cJSON *object,const char *string,cJSON *item);	/* Use this when string is definitely const (i.e. a literal, or as good as), and will definitely survive the cJSON object */
/* Append reference to item to the specified array/object. Use this when you want to add an existing cJSON to a new cJSON, but don't want to corrupt your existing cJSON. */
extern void cJSON_AddItemReferenceToArray(cJSON *array, cJSON *item);
extern void	cJSON_AddItemReferenceToObject(cJSON *object,const char *string,cJSON *item);

/* Remove/Detatch items from Arrays/Objects. */
extern cJSON *cJSON_DetachItemFromArray(cJSON *array,int which);
extern void   cJSON_DeleteItemFromArray(cJSON *array,int which);
extern cJSON *cJSON_DetachItemFromObject(cJSON *object,const char *string);
extern void   cJSON_DeleteItemFromObject(cJSON *object,const char *string);
	
/* Update array items. */
//向array数组的指定位置which插入一个元素newitem
extern void cJSON_InsertItemInArray(cJSON *array,int which,cJSON *newitem);	/* Shifts pre-existing items to the right. */
//用newitem替换array数组which位置的元素
extern void cJSON_ReplaceItemInArray(cJSON *array,int which,cJSON *newitem);
//用newitem替换object元素下键值是string的属性
extern void cJSON_ReplaceItemInObject(cJSON *object,const char *string,cJSON *newitem);

/* Duplicate a cJSON item */
extern cJSON *cJSON_Duplicate(cJSON *item,int recurse);
/* Duplicate will create a new, identical cJSON item to the one you pass, in new memory that will
need to be released. With recurse!=0, it will duplicate any children connected to the item.
The item->next and ->prev pointers are always zero on return from Duplicate. */

/* ParseWithOpts allows you to require (and check) that the JSON is null terminated, and to retrieve the pointer to the final byte parsed. */
extern cJSON *cJSON_ParseWithOpts(const char *value,const char **return_parse_end,int require_null_terminated);

extern void cJSON_Minify(char *json);

//使用宏定义！
/* Macros for creating things quickly. */
#define cJSON_AddNullToObject(object,name)		cJSON_AddItemToObject(object, name, cJSON_CreateNull())
#define cJSON_AddTrueToObject(object,name)		cJSON_AddItemToObject(object, name, cJSON_CreateTrue())
#define cJSON_AddFalseToObject(object,name)		cJSON_AddItemToObject(object, name, cJSON_CreateFalse())
#define cJSON_AddBoolToObject(object,name,b)	cJSON_AddItemToObject(object, name, cJSON_CreateBool(b))
#define cJSON_AddNumberToObject(object,name,n)	cJSON_AddItemToObject(object, name, cJSON_CreateNumber(n))
#define cJSON_AddStringToObject(object,name,s)	cJSON_AddItemToObject(object, name, cJSON_CreateString(s))

/* When assigning an integer value, it needs to be propagated to valuedouble too. */
#define cJSON_SetIntValue(object,val)			((object)?(object)->valueint=(object)->valuedouble=(val):(val))
#define cJSON_SetNumberValue(object,val)		((object)?(object)->valueint=(object)->valuedouble=(val):(val))

//用于让C兼容C++！
#ifdef __cplusplus
}
#endif

#endif
