// Use of this source code is governed by a BSD-style license
// that can be found in the License file.
//
// Author: Shuo Chen (chenshuo at chenshuo dot com)

#include <muduo/base/Thread.h>
#include <muduo/base/CurrentThread.h>
#include <muduo/base/Exception.h>
#include <muduo/base/Logging.h>

#include <boost/static_assert.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/weak_ptr.hpp>

#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <linux/unistd.h>

namespace muduo
{
namespace CurrentThread
{
  __thread int t_cachedTid = 0;        //缓存的线程ID(int)
  __thread char t_tidString[32];       //缓存的线程ID(string)
  __thread int t_tidStringLength = 6;  //
  __thread const char* t_threadName = "unknown";
  const bool sameType = boost::is_same<int, pid_t>::value;
  BOOST_STATIC_ASSERT(sameType);
}

namespace detail
{

//系统调用SYS_gettid获取线程ID
pid_t gettid()
{
  return static_cast<pid_t>(::syscall(SYS_gettid));
}

void afterFork()
{
  muduo::CurrentThread::t_cachedTid = 0;
  muduo::CurrentThread::t_threadName = "main";
  
  /*
   * inline int tid()
   * {
   *   if (__builtin_expect(t_cachedTid == 0, 0))
   *   {
   *     cacheTid();
   *   }
   *   return t_cachedTid;
   * }
   */
  CurrentThread::tid();
  // no need to call pthread_atfork(NULL, NULL, &afterFork);
}

class ThreadNameInitializer
{
 public:
  ThreadNameInitializer()
  {
    muduo::CurrentThread::t_threadName = "main";
    CurrentThread::tid();
    /*
     * ？？？
     */
    pthread_atfork(NULL, NULL, &afterFork);
  }
};

ThreadNameInitializer init;

/*
 * 定义线程需要的数据的结构体
 */
struct ThreadData
{
  typedef muduo::Thread::ThreadFunc ThreadFunc; 
  ThreadFunc func_; //线程方法
  string name_;     //线程名称
  pid_t* tid_;      //线程ID
  CountDownLatch* latch_;  //这是啥？

  //构造方法！
  ThreadData(const ThreadFunc& func,
             const string& name,
             pid_t* tid,
             CountDownLatch* latch)
    : func_(func),
      name_(name),
      tid_(tid),
      latch_(latch)
  { }

  void runInThread()
  {
    //获取当前线程上下文的线程ID
    *tid_ = muduo::CurrentThread::tid();
    tid_ = NULL;
    latch_->countDown();
    latch_ = NULL;

    //设置当前线程的线程名称
    muduo::CurrentThread::t_threadName = name_.empty() ? "muduoThread" : name_.c_str();
    ::prctl(PR_SET_NAME, muduo::CurrentThread::t_threadName);
    try
    {
      //调用线程方法
      func_();
      //线程运行完后，重新设置线程名
      muduo::CurrentThread::t_threadName = "finished";
    }
    catch (const Exception& ex)
    {
      //捕获Exception异常
      muduo::CurrentThread::t_threadName = "crashed";
      fprintf(stderr, "exception caught in Thread %s\n", name_.c_str());
      fprintf(stderr, "reason: %s\n", ex.what());
      fprintf(stderr, "stack trace: %s\n", ex.stackTrace());
      abort();
    }
    catch (const std::exception& ex)
    {
      //捕获std::exception异常
      muduo::CurrentThread::t_threadName = "crashed";
      fprintf(stderr, "exception caught in Thread %s\n", name_.c_str());
      fprintf(stderr, "reason: %s\n", ex.what());
      abort();
    }
    catch (...)
    {
      //捕获其他异常
      muduo::CurrentThread::t_threadName = "crashed";
      fprintf(stderr, "unknown exception caught in Thread %s\n", name_.c_str());
      throw; // rethrow
    }
  }
};

//启动线程
void* startThread(void* obj)
{
  //强转为ThreadData
  ThreadData* data = static_cast<ThreadData*>(obj);
  //调用ThreadData.runInThread()方法启动线程
  data->runInThread();
  //线程运行结束后，释放ThreadData的资源
  delete data;
  return NULL;
}

}
}

using namespace muduo;

/*
 * 缓存线程ID
 */
void CurrentThread::cacheTid()
{
  if (t_cachedTid == 0)
  {
    /* 缓存int类型的线程ID
     *
     * pid_t gettid()
     * {
     *   return static_cast<pid_t>(::syscall(SYS_gettid));
     * }
     */
    t_cachedTid = detail::gettid();
    
    /*
     * 缓存string类型的线程ID
     */
    t_tidStringLength = snprintf(t_tidString, sizeof t_tidString, "%5d ", t_cachedTid);
  }
}

/*
 * 判断是不是主线程
 * 如果线程id等于进程id，则表示其实主线程
 */
bool CurrentThread::isMainThread()
{
  return tid() == ::getpid();
}

/*
 * 
 */
void CurrentThread::sleepUsec(int64_t usec)
{
  struct timespec ts = { 0, 0 };
  ts.tv_sec = static_cast<time_t>(usec / Timestamp::kMicroSecondsPerSecond);
  ts.tv_nsec = static_cast<long>(usec % Timestamp::kMicroSecondsPerSecond * 1000);
  ::nanosleep(&ts, NULL);
}

/*
 * 原子型的int32类型
 */
AtomicInt32 Thread::numCreated_;

Thread::Thread(const ThreadFunc& func, const string& n)
  : started_(false),   //是否启动线程
    joined_(false),    //是否join
    pthreadId_(0),     //线程ID
    tid_(0),           //
    func_(func),       //线程方法
    name_(n),          //线程名称
    latch_(1)
{
  //设置默认的线程名称
  setDefaultName();
}

#ifdef __GXX_EXPERIMENTAL_CXX0X__
Thread::Thread(ThreadFunc&& func, const string& n)
  : started_(false),
    joined_(false),
    pthreadId_(0),
    tid_(0),
    func_(std::move(func)),
    name_(n),
    latch_(1)
{
  setDefaultName();
}

#endif

Thread::~Thread()
{
  if (started_ && !joined_)
  {
    pthread_detach(pthreadId_);
  }
}

void Thread::setDefaultName()
{
  //构造函数中调用setDefaultName();
  //所以在每次创建线程时，都会用numCreated_.incrementAndGet更新当前线程数！
  int num = numCreated_.incrementAndGet();
  if (name_.empty())
  {
    char buf[32];
    snprintf(buf, sizeof buf, "Thread%d", num);
    name_ = buf;
  }
}

//启动线程
void Thread::start()
{
  //断言这时候线程还没有启动
  assert(!started_);
  //设置started_为真，表示线程已经启动
  started_ = true;
  // FIXME: move(func_)
  
  //创建线程数据类对象
  detail::ThreadData* data = new detail::ThreadData(func_, name_, &tid_, &latch_);
  
  //调用pthread_create方法创建线程
  //注意一定要判断pthread_create的所有可能返回值，如此的代码才是鲁棒的
  if (pthread_create(&pthreadId_, NULL, &detail::startThread, data))
  {
    started_ = false;
    delete data; // or no delete?
    LOG_SYSFATAL << "Failed in pthread_create";
  }
  else
  {
    latch_.wait();
    assert(tid_ > 0);
  }
}

int Thread::join()
{
  assert(started_);
  assert(!joined_);
  /*
   * 为什么在调用pthread_join之前将joined设置为true
   * 而不是在pthread_join之后将joined设置为true？
   */
  joined_ = true;
  //pthread_join用来等待一个线程的结束,线程间同步的操作
  return pthread_join(pthreadId_, NULL);
}

