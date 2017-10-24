program snote;

uses
  {2016-02-21：
    这个项目是我在看《Delphi高手突破》这本书的时候，这本书的最后一章有专门以这个实例来进行讲解
    2015-10-17左右的时候，我专门将这本书看了一遍，但是当时因为自己对Delphi的理解不深入、开发经验不够，所以书中很多讲解都不理解，而且这个项目实例也没有看
    而现在经过了自己平时有了几次的项目开发经验、自己平时主动学习和整理Delphi的知识、初步看了转换机框架的代码，对Delphi有了一些理解，所以着手看这个项目
    还是以在代码中添加注释的形式，另外自己多去编译运行程序，尝试自己做一些个性化的修改、添加一些新的功能，来加深自己的理解
    书《Delphi高手突破》这本书的第7章中有关于这个小项目的架构的讲解，包括面向对象的设计、类的封装……
      先将书中的知识认真看一遍，对这个小项目的系统架构有一个整体的把握，然后再去结合书看代码细节
      类的封装技巧、Delphi的消息机制、Delphi的控件开发……知识都是可以通过这个项目学习到的！
    所以对于这个项目，一方面是一定要将《Delphi高手突破》这本书放在身边认真看，另外就是结合书去深入研究这个项目的代码细节
  }
  {每个单元前面的序号表示研究这个项目源码我依次看的单元的顺序
  2016-02-22：
    看到了WorkSpaceMgr单元的代码实现；另外GlobalObject单元里需要创建和销毁的所有变量还没有进行注释，明天继续
  2016-02-33 23:30：
    结合《Delphi高手突破》将这个小项目的源码看了一遍，对于整个系统有了一个把握
    但是还是没有对每一个细节都理解，所以还需要自己再去逐个细节逐个细节的看，并且尝试修改程序-->编译-->运行-->测试-->再修改-->……
    }
  Forms,
  UMainForm in 'UMainForm.pas' {MainForm},   //16.编辑器主窗体，主要是放置控件的设计，其余的功能算法逻辑在其他各个单元里面封装，在此单元放置控件、调用其他功能逻辑
  Editor in 'Editor.pas',                    //2.定义了TssnEditor类，“编辑器组件”
  ssnPublic in 'ssnPublic.pas',              //6.定义了一些常量，GetExePath函数
  WorkSpaceMgr in 'WorkSpaceMgr.pas',        //8.工作区管理器
  WorkSpace in 'WorkSpace.pas',              //5.定义了TssnWorkSpace类，“工作区”，作为TssnEditor的代理
  TabWorkSpaceMgr in 'TabWorkSpaceMgr.pas',  //9.TssnWorkSpace、TssnWorkSpaceMgr都是抽象类，这个单元分别为其实现了具体的子类
  GlobalObject in 'GlobalObject.pas',        //8--16.定义了一些全局变量（对象……）的初始化方法、释放方法
  EditorCtor in 'EditorCtor.pas',            //10.编辑器组件的“构造器”
  MemoEditor in 'MemoEditor.pas',            //3.定义了TssnMemoEditor类，继承自TssnEditor类
  WorkSpaceMgrCtor in 'WorkSpaceMgrCtor.pas',//11.工作区管理器的“构造器”
  MultiLan in 'MultiLan.pas',                //17.该单元是为了实现多语言版本而准备的
  InterActive in 'InterActive.pas',          //15.封装了用于用户交互的类，主要是各种弹出框
  IntfEditor in 'IntfEditor.pas',            //1.定义了IssnEditor抽象类，TssnEditor和TssnWorkSpace都继承自它，保证TssnWorkSpace可以正确代理TssnEditor
  SettingMgr in 'SettingMgr.pas',            //14.默认设置管理
  EditorEvent in 'EditorEvent.pas',          //12.编辑器组件的事件委托
  WorkSpaceEvent in 'WorkSpaceEvent.pas',    //13.工作区的事件委托
  RichEditor in 'RichEditor.pas';            //4.定义了TssnRichEditor类，继承自TssnEditor，实现一个简单的富文本编辑器组件

{$R *.RES}

begin
    Application.Initialize;
    try
        InitObjects();    //任务开始时，先创建所有需要的对象，包括创建主窗体等的代码都放在这里了
        Application.Title := 'Sunny SmartNote 5';
        Application.Run;  //通过单步调试，对于Delphi项目，其实运行起开就会单步停止在这句代码
                          //Application.Run()就是进入主消息循环，响应各种消息，包括用户操作程序界面发出的消息……
    finally
        UnInitObjects();  //任务结束时，释放通过InitObjects创建的对象
    end;
end.
