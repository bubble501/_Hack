// Use of this source code is governed by a BSD-style license
// that can be found in the License file.
//
// Author: Shuo Chen (chenshuo at chenshuo dot com)

#ifndef MUDUO_BASE_THREAD_H
#define MUDUO_BASE_THREAD_H

#include <muduo/base/Atomic.h>
#include <muduo/base/CountDownLatch.h>
#include <muduo/base/Types.h>

#include <boost/function.hpp>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <pthread.h>

namespace muduo
{

class Thread : boost::noncopyable
{
 public:
  //void ThreadFunc();
  typedef boost::function<void ()> ThreadFunc;

  /*
   * C++中的explicit关键字只能用于修饰只有一个参数的类构造函数
   * 它的作用是表明该构造函数是显式的、非隐式的
   * 跟它相对应的另一个关键字implicit，意思是隐藏的
   * 类构造函数默认情况下声明为implicit
   * http://www.cnblogs.com/ymy124/p/3632634.html
   * explicit关键字的作用就是防止类构造函数的隐式自动转换.
   */
  explicit Thread(const ThreadFunc&, const string& name = string());
#ifdef __GXX_EXPERIMENTAL_CXX0X__
  explicit Thread(ThreadFunc&&, const string& name = string());
#endif
  ~Thread();

  void start();
  int join(); // return pthread_join()

  bool started() const { return started_; }
  // pthread_t pthreadId() const { return pthreadId_; }
  pid_t tid() const { return tid_; }
  const string& name() const { return name_; }

  //类方法，获取创建了的线程数
  static int numCreated() { return numCreated_.get(); }

 private:
  void setDefaultName();

  bool       started_;
  bool       joined_;
  pthread_t  pthreadId_;
  pid_t      tid_;
  ThreadFunc func_;      //线程方法
  string     name_;      //线程名称
  CountDownLatch latch_; //

  //缓存获取创建了的线程数
  static AtomicInt32 numCreated_;
};

}
#endif
