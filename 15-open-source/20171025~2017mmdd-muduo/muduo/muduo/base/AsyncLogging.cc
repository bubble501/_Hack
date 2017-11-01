#include <muduo/base/AsyncLogging.h>
#include <muduo/base/LogFile.h>
#include <muduo/base/Timestamp.h>

#include <stdio.h>

using namespace muduo;

//构造方法
AsyncLogging::AsyncLogging(const string& basename,
                           size_t rollSize,
                           int flushInterval)
  : flushInterval_(flushInterval),
    running_(false),
    basename_(basename),
    rollSize_(rollSize),
    thread_(boost::bind(&AsyncLogging::threadFunc, this), "Logging"),  //指定线程方法
    latch_(1),
    mutex_(),
    cond_(mutex_),
    currentBuffer_(new Buffer),
    nextBuffer_(new Buffer),
    buffers_()
{
  currentBuffer_->bzero();
  nextBuffer_->bzero();
  buffers_.reserve(16);
}

//异步方式增加一个日志记录
void AsyncLogging::append(const char* logline, int len)
{
  /*
   * C++作用域有这样的一个属性
   * 在创建一个类对象时，会调用其构造方法
   * 在创建对象对应的作用域结束时，变量失效，会自动调用其析构方法！
   * 所以这里在MutexLockGuard的构造方法中进行加锁
   * 在MutexLockGuard的析构方法中做解锁的调用
   */
  muduo::MutexLockGuard lock(mutex_);
  
  //typedef muduo::detail::FixedBuffer<muduo::detail::kLargeBuffer> Buffer;
  //typedef boost::ptr_vector<Buffer> BufferVector;
  //typedef BufferVector::auto_type BufferPtr;
  //BufferPtr currentBuffer_;
  //这里判断如果当前缓冲区的可用空间大于len，那么就将这个日志放到当前的缓冲区
  if (currentBuffer_->avail() > len)
  {
    currentBuffer_->append(logline, len);
  }
  else
  {
    //BufferVector buffers_;
    buffers_.push_back(currentBuffer_.release());

    //BufferPtr nextBuffer_;
    if (nextBuffer_)
    {
      currentBuffer_ = boost::ptr_container::move(nextBuffer_);
    }
    else
    {
      currentBuffer_.reset(new Buffer); // Rarely happens
    }
    currentBuffer_->append(logline, len);
    cond_.notify();
  }
}

//异步写日志线程方法
void AsyncLogging::threadFunc()
{
  assert(running_ == true);
  latch_.countDown();

  //定义日志输出
  LogFile output(basename_, rollSize_, false);
  
  BufferPtr newBuffer1(new Buffer);
  BufferPtr newBuffer2(new Buffer);
  newBuffer1->bzero();
  newBuffer2->bzero();
  BufferVector buffersToWrite;
  buffersToWrite.reserve(16);

  //线程循环
  while (running_)
  {
    assert(newBuffer1 && newBuffer1->length() == 0);
    assert(newBuffer2 && newBuffer2->length() == 0);
    assert(buffersToWrite.empty());

    {
      /*
       * 同理，利用C++类对象的作用域对构造和析构方法的调用，来自动化管理锁！
       */
      muduo::MutexLockGuard lock(mutex_);
      if (buffers_.empty())  // unusual usage!
      {
        cond_.waitForSeconds(flushInterval_);
      }
      buffers_.push_back(currentBuffer_.release());
      currentBuffer_ = boost::ptr_container::move(newBuffer1);
      buffersToWrite.swap(buffers_);
      if (!nextBuffer_)
      {
        nextBuffer_ = boost::ptr_container::move(newBuffer2);
      }
    }

    assert(!buffersToWrite.empty());

    if (buffersToWrite.size() > 25)
    {
      char buf[256];
      snprintf(buf, sizeof buf, "Dropped log messages at %s, %zd larger buffers\n",
               Timestamp::now().toFormattedString().c_str(),
               buffersToWrite.size()-2);
      fputs(buf, stderr);
      output.append(buf, static_cast<int>(strlen(buf)));
      buffersToWrite.erase(buffersToWrite.begin()+2, buffersToWrite.end());
    }

    for (size_t i = 0; i < buffersToWrite.size(); ++i)
    {
      // FIXME: use unbuffered stdio FILE ? or use ::writev ?
      output.append(buffersToWrite[i].data(), buffersToWrite[i].length());
    }

    if (buffersToWrite.size() > 2)
    {
      // drop non-bzero-ed buffers, avoid trashing
      buffersToWrite.resize(2);
    }

    if (!newBuffer1)
    {
      assert(!buffersToWrite.empty());
      newBuffer1 = buffersToWrite.pop_back();
      newBuffer1->reset();
    }

    if (!newBuffer2)
    {
      assert(!buffersToWrite.empty());
      newBuffer2 = buffersToWrite.pop_back();
      newBuffer2->reset();
    }

    buffersToWrite.clear();
    output.flush();
  }
  output.flush();
}

