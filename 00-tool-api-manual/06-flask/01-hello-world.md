目录结构如下

```
mkdir app
mkdir app/static
mkdir app/templates
mkdir tmp
```

为app包创建一个初始化脚本（app/\_\_init\_\_.py）

```
from flask import Flask

app = Flask(__name__)
from app import views

# 该脚本简单地创建应用对象
# 接着导入视图模块，该模块暂未使用
```

编写视图函数（app/views.py）

```
from app import app

# 两个route装饰器创建了从网址/以及/index到这个函数的映射
@app.route('/')
@app.route('/index')
def index():
    return "hello world !"
```

在app根目录下创建run.py脚本

```
#!flask/bin/python
from app import app
app.run(debug = True)
```

这个脚本简单地从app包中导入app变量并调用它的run方法来启动服务器

`chmod a+x run.py`之后即可通过`./run.py`直接运行脚本

同时可以直接通过`Ctrl-C`来终止服务器

>这里创建了一个最简单的Python Web程序，完全没有考虑到高并发等复杂场景下的情况

