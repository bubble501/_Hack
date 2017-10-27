// Concurrent downloading one file from HTTP

#include <examples/curl/Curl.h>
#include <muduo/base/Logging.h>
#include <muduo/net/EventLoop.h>
#include <boost/bind.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <stdio.h>
#include <sstream>

using namespace muduo;
using namespace muduo::net;

//智能指针的使用（shared_ptr支持引用计数）
typedef boost::shared_ptr<FILE> FilePtr;

//判断str是不是以prefix[N]这个字符串开头的
template<int N>
bool startWith(const string& str, const char (&prefix)[N])
{
  /*
   * template< class InputIt1, class InputIt2 >
   * bool equal( InputIt1 first1, InputIt1 last1, 
   *             InputIt2 first2 )
   */
  return str.size() >= N-1 && std::equal(prefix, prefix+N-1, str.begin());
}

//单例类
class Piece : boost::noncopyable
{
 public:
  Piece(const curl::RequestPtr& req,         //HTTP请求
        const FilePtr& out,                  //
        const muduo::string& range,          //
        const boost::function<void()> done)  //
    : req_(req),
      out_(out),
      range_(range),
      doneCb_(done)
  {
    LOG_INFO << "range: " << range;
    req->setRange(range);
    req_->setDataCallback(
        boost::bind(&Piece::onData, this, _1, _2));
    req_->setDoneCallback(
        boost::bind(&Piece::onDone, this, _1, _2));
  }
 private:
  //收到数据的回调，将数据写到文件
  void onData(const char* data, int len)
  {
    ::fwrite(data, 1, len, get_pointer(out_));
  }

  //请求完成时的回调
  void onDone(curl::Request* c, int code)
  {
    LOG_INFO << "[" << range_ << "] is done";
    req_.reset();
    out_.reset();
    doneCb_();
  }

  curl::RequestPtr req_;
  FilePtr out_;
  muduo::string range_;
  
  
 /*
  * 理解被存函数的最佳方法就是把它想象成为一个普通的函数对象，该函数对象用于封装另一个函数（或函数对象）
  * 这个被存的函数的最大用途是它可以被多次调用，而无须在创建function时立即使用
  * 在声明function时，声明中最重要的部分是函数的签名
  * 这部分即是告诉function它将保存的函数或函数对象的签名和返回类型
  * 我们已经看到，有两种方式来执行这个声明
  * 下面有一个完整的程序，程序声明了一个boost::function
  * 它可以保存返回bool（或某个可以隐式转换为bool的类型）
  * 并接受两个参数的类函数实体，第一个参数转换为int，第二个转换为double

```
#include <iostream>
#include "boost/function.hpp"

bool some_func(int i, double d){
	return i > d;
}

int main(){
	boost::function<bool (int, double)> f;
	f = &some_func;
	f(10, 1.1);
}
```

  * 当function f首次创建时，它不保存任何函数
  * 它是空的，可以在一个布尔上下文中进行测试
  * 如果你试图调用一个没有保存任何函数或函数对象的function
  * 它将抛出一个类型bad_function_call的异常
  * 为了避免这个问题，我们用普通的赋值语法把一个指向some_func的指针赋给f
  * 这导致f保存了到some_func的指针
  * 最后我们用参数10(int)和1.1(double)来调用f(用函数调用操作符)
  * 要调用一个function，你必须提供被存函数或函数对象所期望的准确数量的参数
  
  ? 不过为什么要用boost::function而不是直接用函数指针或者函数对象指针呢
  */
  boost::function<void()> doneCb_;
};

//单例类
class Downloader : boost::noncopyable
{
 public:
  Downloader(EventLoop* loop, const string& url)
    : loop_(loop),
      curl_(loop_),
      url_(url),
      req_(curl_.getUrl(url_)),
      found_(false),
      acceptRanges_(false),
      length_(0),
      pieces_(kConcurrent),
      concurrent_(0)
  {
    //收到文件头的回调
    req_->setHeaderCallback(
        boost::bind(&Downloader::onHeader, this, _1, _2));
    //文件头收完时的回调
    req_->setDoneCallback(
        boost::bind(&Downloader::onHeaderDone, this, _1, _2));
    req_->headerOnly();
  }

 private:
  //收到HTTP应答头的回调
  void onHeader(const char* data, int len)
  {
    string line(data, len);
    /*
     HTTP的应答报文格式如下
     版本  空格  状态码  空格  原因短语  回车符  换行符
     头部域名称:头部域值                 回车符  换行符
     ...
     头部域名称:头部域值                 回车符  换行符
     回车符  换行符
     附属体
     */
    if (startWith(line, "HTTP/1.1 200") || startWith(line, "HTTP/1.0 200"))
    {
      found_ = true;
    }
    if (line == "Accept-Ranges: bytes\r\n")
    {
      acceptRanges_ = true;
      LOG_DEBUG << "Accept-Ranges";
    }
    else if (startWith(line, "Content-Length:"))
    {
      length_ = atoll(line.c_str() + strlen("Content-Length:"));
      LOG_INFO << "Content-Length: " << length_;
    }
  }

  //HTTP应答收完的回调
  void onHeaderDone(curl::Request* c, int code)
  {
    LOG_DEBUG << code;
    if (acceptRanges_ && length_ >= kConcurrent * 4096)
    {
      LOG_INFO << "Downloading with " << kConcurrent << " connections";
      concurrent_ = kConcurrent;
      concurrentDownload();
    }
    else if (found_)
    {
      LOG_WARN << "Single connection download";
      FILE* fp = ::fopen("output", "wb");
      if (fp)
      {
        FilePtr(fp, ::fclose).swap(out_);
        req_.reset();
        req2_ = curl_.getUrl(url_);
        req2_->setDataCallback(
            boost::bind(&Downloader::onData, this, _1, _2));
        req2_->setDoneCallback(
            boost::bind(&Downloader::onDownloadDone, this));
        concurrent_ = 1;
      }
      else
      {
        LOG_ERROR << "Can not create output file";
        loop_->quit();
      }
    }
    else
    {
      LOG_ERROR << "File not found";
      loop_->quit();
    }
  }

  void concurrentDownload()
  {
    const int64_t pieceLen = length_ / kConcurrent;
    for (int i = 0; i < kConcurrent; ++i)
    {
      char buf[256];
      snprintf(buf, sizeof buf, "output-%05d-of-%05d", i, kConcurrent);
      FILE* fp = ::fopen(buf, "wb");
      if (fp)
      {
        FilePtr out(fp, ::fclose);
        curl::RequestPtr req = curl_.getUrl(url_);

        std::ostringstream range;
        if (i < kConcurrent - 1)
        {
          range << i * pieceLen << "-" << (i+1) * pieceLen - 1;
        }
        else
        {
          range << i * pieceLen << "-" << length_ - 1;
        }
        pieces_.push_back(new Piece(req,
                                    out,
                                    range.str().c_str(), // std::string -> muduo::string
                                    boost::bind(&Downloader::onDownloadDone, this)));
      }
      else
      {
        LOG_ERROR << "Can not create output file: " << buf;
        loop_->quit();
      }
    }
  }

  void onData(const char* data, int len)
  {
    ::fwrite(data, 1, len, get_pointer(out_));
  }

  //下载完成即可退出EventLoop循环
  void onDownloadDone()
  {
    if (--concurrent_ <= 0)
    {
      loop_->quit();
    }
  }

  EventLoop* loop_;         //EventLoop
  curl::Curl curl_;         //curl
  string url_;              //url字符串
  curl::RequestPtr req_;    //请求
  curl::RequestPtr req2_;   //请求
  bool found_;
  bool acceptRanges_;
  int64_t length_;
  FilePtr out_;             //用于输出的文件指针
  boost::ptr_vector<Piece> pieces_;   //存储Piece智能指针的Vector
  int concurrent_;

  const static int kConcurrent = 4;
};

int main(int argc, char* argv[])
{
  EventLoop loop;  //定义一个EventLoop
  curl::Curl::initialize(curl::Curl::kCURLssl);
  //定义URL字符串
  string url = argc > 1 ? argv[1] : "https://chenshuo-public.s3.amazonaws.com/pdf/allinone.pdf";
  //定义下载器
  Downloader d(&loop, url);
  //开始EventLoop循环
  loop.loop();
}
