#ifndef MUDUO_BASE_ASYNCLOGGING_H
#define MUDUO_BASE_ASYNCLOGGING_H

#include <muduo/base/BlockingQueue.h>
#include <muduo/base/BoundedBlockingQueue.h>
#include <muduo/base/CountDownLatch.h>
#include <muduo/base/Mutex.h>
#include <muduo/base/Thread.h>
#include <muduo/base/LogStream.h>

#include <boost/bind.hpp>
#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

namespace muduo
{

/*
 * AsyncLogging是单例类
 * AsyncLogging通过名字看出来其是异步处理日志的
 */
class AsyncLogging : boost::noncopyable
{
 public:

  //构造函数
  AsyncLogging(const string& basename,  //
               size_t rollSize,         //
               int flushInterval = 3);  //

  //析构函数
  ~AsyncLogging()
  {
    if (running_)
    {
      stop();
    }
  }

  //新增一行日志记录
  void append(const char* logline, int len);

  //启动异步写日志线程
  void start()
  {
    running_ = true;
    thread_.start();
    latch_.wait();
  }

  //停止异步写日志线程
  void stop()
  {
    running_ = false;
    cond_.notify();
    thread_.join();
  }

 private:

  // declare but not define, prevent compiler-synthesized functions
  AsyncLogging(const AsyncLogging&);  // ptr_container
  void operator=(const AsyncLogging&);  // ptr_container

  //异步写日志线程方法
  void threadFunc();

  typedef muduo::detail::FixedBuffer<muduo::detail::kLargeBuffer> Buffer;
  typedef boost::ptr_vector<Buffer> BufferVector;
  typedef BufferVector::auto_type BufferPtr;

  const int flushInterval_;
  bool running_;
  string basename_;
  size_t rollSize_;
  muduo::Thread thread_;        //异步写日志线程
  muduo::CountDownLatch latch_; //
  muduo::MutexLock mutex_;      //Muduo对Linux mutex进行的封装，用于线程同步
  muduo::Condition cond_;       //
  BufferPtr currentBuffer_;     //
  BufferPtr nextBuffer_;        //
  BufferVector buffers_;        //
};

}
#endif  // MUDUO_BASE_ASYNCLOGGING_H
