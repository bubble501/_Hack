#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/types.h>
#include "Oracle.h"
#include "MSServer.h"
#include "./cLockQueue/cLockQueue.h"

//注意错误情况下的资源释放！
//测试情况：目前可以取委托，但是更新委托状态却有问题，返回更新了1条，但实际上没有更新
//                    没有更新，不过后续取委托也确实取不出来了，是不是内存更新了但是磁盘没有更新，Proc的特性是什么样的？
//队列的线程安全、List的封装还需再做测试
//考虑控制数量，否则可能导致队列过长、内存过大！

//性能测试：取委托能达到：24000笔/s，比Windows/Delphi/ADO的600快得多了
//                    报单能达到：

void *StartSendEntrust(void *arg);
void Stop(int signo);
int isClose = 0;
CLockQueue *EntrustQueue = NULL;

int main()
{
    int i = 0;
    int initRet = 0;
    int EntCount = 0;
    int secondEntCount = 0;
    EntrustData *tmpEntData;

    printf("开始注册Ctrl-C信号\n");
    signal(SIGINT, Stop); 

    printf("连接Oracle\n");
    initRet = OracleInit("trade", "trade");
    if(0 != initRet){
        printf("初始化Oracle连接错误\n");
        return -1;
    }

    printf("连接SQLServer\n");
    initRet = MSServerInit("sa", "Xumeng@13245", "192.168.163.41:1433");
    if(0 != initRet){
        printf("初始化MSServer连接错误\n");
        return -1;
    }

    printf("准备取委托数组\n");
    EntrustData* EntrustDataList[MAXCOUNT];
    for(i=0; i<MAXCOUNT; i++){
        EntrustDataList[i] = (EntrustData *)malloc(sizeof(EntrustData));
    }

    printf("初始化委托队列\n");
    EntrustQueue = clockqueue_init(NULL);
    
    printf("启动子线程\n");
    pthread_t aThread;
    int res = pthread_create(&aThread, NULL, StartSendEntrust, (void *)"测试子进程");

    printf("主线程开始取委托\n");
    struct timeval tvStart, tvEnd;
    gettimeofday(&tvStart, NULL);
    while(0 == isClose){
        EntCount = GetEntrusts('1', &EntrustDataList, MAXCOUNT);
        for(i=0; i<EntCount; i++){
            tmpEntData = (EntrustData *)malloc(sizeof(EntrustData));
            *tmpEntData = *EntrustDataList[i];

            clockqueue_enqueue(EntrustQueue, (void *)tmpEntData);
        }

        //统计每秒取出多少笔委托
        secondEntCount = secondEntCount + EntCount;
        gettimeofday(&tvEnd, NULL);
        if((tvEnd.tv_sec * ONE_SECOND + tvEnd.tv_usec - (tvStart.tv_sec * ONE_SECOND + tvStart.tv_usec)) > 2 * ONE_SECOND){
            printf("每2秒取出%d笔委托\n", secondEntCount);
            gettimeofday(&tvStart, NULL);
            secondEntCount = 0;
        }

        if(EntCount <= 0){
            usleep(10000);
        }
    }

    printf("等待子线程结束\n");
    void *thread_result;
    res = pthread_join(aThread, &thread_result);    

    printf("主线程结束\n");

    return 0;
}

void *StartSendEntrust(void *arg)
{
    EntrustData *tmpEntData = NULL;
    int secondEntCount = 0;
    long usetime = 0;
    printf("子线程开始报单\n");

    struct timeval tvStart, tvEnd;
    gettimeofday(&tvStart, NULL);
    while(0 == isClose){
        clockqueue_dequeue(EntrustQueue, (void **)(&tmpEntData));
        if(NULL == tmpEntData){
            usleep(10000);
            continue;
        }
        SendEntrust(tmpEntData, "oiw03..ashare_ordwth");
        free(tmpEntData);
        
        //报单线程判断处理2000笔用的时间，不要每次都调用gettimeofday，尽可能保证性能统计不对运行产生影响！
        secondEntCount = secondEntCount + 1;
        if(secondEntCount >= 2000){
            gettimeofday(&tvEnd, NULL);
            usetime = tvEnd.tv_sec * ONE_SECOND + tvEnd.tv_usec - (tvStart.tv_sec * ONE_SECOND + tvStart.tv_usec);
            printf("报送2000*10笔委托耗时%ld ms\n", usetime/1000);
            secondEntCount = 0;
            gettimeofday(&tvStart, NULL);
        }
    }
    printf("子线程报单结束\n");
}

void Stop(int signo)
{
    printf("收到Ctrl-C信号，将停止线程\n");
    isClose = 1;
}
