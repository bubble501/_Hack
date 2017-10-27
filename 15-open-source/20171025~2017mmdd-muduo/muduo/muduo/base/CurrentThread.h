// Use of this source code is governed by a BSD-style license
// that can be found in the License file.
//
// Author: Shuo Chen (chenshuo at chenshuo dot com)

#ifndef MUDUO_BASE_CURRENTTHREAD_H
#define MUDUO_BASE_CURRENTTHREAD_H

#include <stdint.h>

namespace muduo
{
namespace CurrentThread
{
  //__thread关键字是GCC内置的线程局部存储设施
  //存取效率可以和全局变量相比
  //__thread变量每一个线程有一份独立实体，各个线程的值互不干扰
  //类似于Delphi中的threadvar
  extern __thread int t_cachedTid;          //用来缓存线程ID
  /*
   * 将线程ID以字符串的形式缓存起来，方便输出日志的时候快速获取
   * 以及其他情况下需要以字符串方式获取线程ID
   * 这样就可以在使用时不需要再去调用函数获取int的ID，以及调用函数去将int转成string
   * 以空间换时间的典型应用场景
   */
  extern __thread char t_tidString[32];     
  extern __thread int t_tidStringLength;    //
  extern __thread const char* t_threadName; //线程名称
  void cacheTid();

  inline int tid()
  {
    /*
     * long __builtin_expect(long exp, long c);
     * 由于大部分程序员在分支预测方面做得很糟糕
     * 所以GCC提供了这个內建函数来帮助程序员处理分支预测
     * 你期望exp表达式的值等于常数c
     * 看c的值，如果c的值为0（即期望的函数返回值），那么执行if分支的可能性小
     * 否则执行else分支的可能性小（函数的返回值等于第一个参数exp）
     * GCC在编译过程中，会将可能性更大的代码紧跟着前面的代码，从而减少指令跳转带来的性能下降，达到优化程序的目的
     * 通常，你也许会更喜欢使用gcc的一个参数`-fprofile-arcs`来收集程序运行的关于执行流程和分支走向的实际反馈信息
     * 但是对于很多程序来说，数据是很难收集的
     */
    /*
     * 下面的函数，如果发现线程ID没有缓存，那么先获取线程ID
     * 获取线程ID后，分别以int、string将其缓存
     * 最后返回当前线程的id
     * 因为t_cachedTid是__thread修饰的，是每个线程互相独立的
     */
    if (__builtin_expect(t_cachedTid == 0, 0))
    {
      /*
       * void CurrentThread::cacheTid()
       * {
       *   if (t_cachedTid == 0)
       *   {
       *     t_cachedTid = detail::gettid();
       *     t_tidStringLength = snprintf(t_tidString, sizeof t_tidString, "%5d ", t_cachedTid);
       *   }
       * }
       */
      cacheTid();
    }
    return t_cachedTid;
  }

  //获取字符串格式的线程id
  inline const char* tidString() // for logging
  {
    return t_tidString;
  }

  inline int tidStringLength() // for logging
  {
    return t_tidStringLength;
  }

  //获取线程名称
  inline const char* name()
  {
    return t_threadName;
  }

  //判断是不是主线程？
  bool isMainThread();

  //？？？
  void sleepUsec(int64_t usec);
}
}

#endif
