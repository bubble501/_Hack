#include <stdio.h>
#include <pthread.h>

struct char_print_params
{
	char character;
	int count;
};

//线程运行函数的声明必须按照要求的格式：void* thread_function(void *)
void* char_print(void* parameters)
{
	struct char_print_params* p = (struct char_print_params* )parameters;
	int i;
	for(i=0; i<p->count; i++)
	{
		fputc(p->character, stderr);
	}

	return NULL;
}

int main()
{
	pthread_t thread1_id;
	pthread_t thread2_id;
	
	struct char_print_params thread1_args;
	struct char_print_params thread2_args;

	thread1_args.character = 'x';
	thread1_args.count = 300;
	/*pthread_create的参数
	 * 第一个参数是输出参数，是指针类型，是指向线程号的指针
	 * 第二个参数用来设置线程属性
	 * 第三个是该线程运行函数的函数指针，也就是线程运行函数的起始地址
	 * 第四个是传给函数运行函数的参数，如果要传递多个，则可以通过定义一个结构体有多个变量来解决！*/
	pthread_create(&thread1_id, NULL, &char_print, &thread1_args);

	thread2_args.character = 'o';
	thread2_args.count = 200;
	pthread_create(&thread2_id, NULL, &char_print, &thread2_args);

	/*pthread_join()方法的作用是调用pthread_join的线程等待对应的线程运行结束*/
	pthread_join(thread1_id, NULL);
	pthread_join(thread2_id, NULL);

	return 0;
}
