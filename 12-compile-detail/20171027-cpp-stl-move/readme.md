>转载自[《C++11右值引用和std::move语句实例解析》](http://www.cnblogs.com/ldlchina/p/6608154.html)

>另外推荐相关文章：[《C++11:深入理解右值引用，move语义和完美转发》](http://blog.csdn.net/booirror/article/details/45057689)

右值引用（及其支持的Move语意和完美转发）是C++0x加入的重大语言特性质疑，从实践角度看，它能够完美解决C++中长久以来为人所诟病的临时对象效率问题。从语言本身讲，它健全了C++中的引用类型在左值右值方面的缺陷。从库设计者的角度讲，它给库设计者又带来了一把利器。从库使用者的角度讲，不动一兵一卒便可以获得“免费的”效率提升……

下面用实例来深入探讨**右值引用**

## 什么是左值，什么是右值

简单说左值可以赋值，右值不可以赋值。以下面的代码为例，`A a = getA();`该语句中a是左值，getA()的返回值是右值

```
#include "stdafx.h"
#include <iostream>

class A{
  public:
    //构造函数
    A() { std::cout << "Constructor" << std::endl; }
    //拷贝构造函数
    A(const A&) { std::cout << "Copy Constructor" << std::endl; }
    //析构函数
    ~A() { }
};

static A getA(){
  A a;
  return a;
}

int main(){
  A a = getA();

  return 0;
}
```

运行上面的代码，输出结果如下

```
Constrcutor
Copy Constructor
```

可以看到A的构造函数调用一次，拷贝构造函数调用了一次，构造函数和拷贝构造函数是消耗比较大的，这里是否可以避免拷贝构造？C++11做到了这一点

## 添加A的移动构造函数

代码如下

```
#include "stdafx.h"
#include <iostream>

class A{
  public:
    A() { std::cout << "Constructor" << std::endl; }
    A(const A&) { std::cout << "Copy Constructor" << std::endl; }
    //移动构造函数
    A(const A&&) { std::cout << "Move Constructor" << std::endl; }
    ~A() {  }
};

static A getA(){
  A a;

  return a;
}

int main(){
  A a = getA();

  return 0;
}
```

运行以上代码，输出结果：

```
Constructor
Move Constructor
```

这样就没有调用拷贝构造函数，而是调用移动构造。这里并没有看到移动构造的优点

## 为A添加一个成员变量

```
#include "stdafx.h"
#include <iostream>
#include <vector>

class B{
  public:
    B() { }
    B(const B&) { std::cout << "B Constructor" << std::endl; }
};

class A{
  public:
    A(): m_b(new B()) { std::cout << "A Constructor" << std::endl; }
    A(const A& src):
         m_b(new B(*(src.mb)))
    {
      std::cout << "A Copy Constructor" << std::endl;
    }
    A(A&& src):
         m_b(src.m_b)
    {
      //这里岂不是导致src的m_b为空指针？？
      //C++ 11中使用nullptr来表示空指针
      //建议阅读：http://www.cnblogs.com/porter/p/3611718.html
      src.m_b = nullptr;
      std::cout << "A Move Constructor" << std::endl;
    }
    ~A() { delete m_b; }
  private:
    B* m_b;
};

static A getA(){
  A a;
  std::cout << "================================================" << std::endl;
  
  return a;
}

int main(){
  A a = getA();
  std::cout << "================================================" << std::endl;
  
  A a1(a);

  return 0;
}
```

运行以上代码，输出结果：

```
A Constructor
================================================
A Move Constructor
================================================
B Constructor
A Copy Constructor
```

`A a = getA();`调用的是A的移动构造，`A al(a);`调用的是A的拷贝构造。A的拷贝构造需要对成员变量B进行深拷贝，而A的移动构造不需要，很明显，A的移动构造效率高

## std::move语句将左值变成右值避免拷贝构造

```
#include "stdafx.h"
#include <iostream>
#include <vector>

class B{
  public:
    B() {}
    B(const B&) { std::cout << "B Constructor" << std::endl; }
};

class A{
  public:
    A(): m_b(new B()) { std::cout << "A Constructor" << std::endl; }
    A(const A& src):
         m_b(new B(*(src.m_b)))
    {
      std::cout << "A Copy Constructor" << std::endl;
    }
    A(A&& src):
          m_b(src.m_b)
    {
      src.m_b = nullptr;
      std::cout << "A Move Constructor" << std::endl;
    }
    ~A() { delete m_b; }
  private:
    B* m_b;
};

static A getA(){
  A a;
  std::cout << "================================================" << std::endl;
  return a;
}

int main(){
  A a = getA();
  std::cout << "================================================" << std::endl;

  A a1(a);
  std::cout << "================================================" << std::endl;

  A a2(std::move(a1));

  return 0;
}
```

运行以上代码，输出结果

```
A Constructor
================================================
A Move Constructor
================================================
B Constructor
A Copy Constructor
================================================
A Move Constructor
```

`A a2(std::move(a1));`将a1转换为右值，因此a2调用的移动构造而不是拷贝构造

## 赋值操作符也可以是移动赋值

```
#include "stdafx.h"
#include <iostream>
#include <vector>

class B{
  public:
    B() {}
    B(const B&) { std::cout << "B Constructor" << std::endl; }
};

class A{
  public:
    A(): m_b(new B()) { std::cout << "A Constructor" << std::endl; }
    A(const A& src):
         m_b(new B(*(src.m_b)))
    {
      std::cout << "A Copy Constructor" << std::endl;
    }
    A(A&& src):
         m_b(src.m_b)
    {
      src.m_b = nullptr;
      std::cout << "A Move Constructor" << std::endl;
    }

    A& operator=(const A& src){
      if(this == &src)
        return *this;
      m_b = new B(*(src.m_b));
      std::cout << "operator=(const A& src)" << std::endl;
      return *this;  
    }
    A& operator=(A&& src){
      if(this == &src)
        return *this;
      m_b = src.m_b;
      src.m_b = nullptr;
      std::cout << "operator=(const A&& src)" << std::endl;
      return *this;
    }

    ~A() { delete m_b; }

  private:
    B* m_b;
};

static A getA(){
  A a;
  std::cout << "================================================" << std::endl;
  return a;
}

int main(){
  A a = getA();  //移动构造
  std::cout << "================================================" << std::endl;
  
  A a1(a);       //拷贝构造
  std::cout << "================================================" << std::endl;

  A a2(std::move(a1));   //移动构造
  std::cout << "================================================" << std::endl;

  a2 = getA();   //移动赋值
  std::cout << "================================================" << std::endl;

  a2 = a1;       //拷贝赋值

  return 0;
}
```

运行以上代码，输出结果：

```
A Constructor
================================================
A Move Constructor
================================================
B Constructor
A Copy Constructor
================================================
A Move Constructor
================================================
A Constructor
================================================
A Move Constructor
operator=(const A&& src)
================================================
B Constructor
operator=(const A& src)
```

>总之尽量给类添加移动构造和移动赋值函数，而减少拷贝构造和拷贝赋值的消耗
