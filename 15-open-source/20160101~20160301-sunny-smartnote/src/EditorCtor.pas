////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   EditorCtor.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit EditorCtor;

{上面实现的IssnEditor、TssnEditor、TssnMemoEditor、TssnWorkSpace、TssnWorkSpaceMgr、TssnTabWorkSpace、TssnTabWorkSpaceMgr组合在一起
  已经是可以运行的一个子系统了
  作为Sunny SmartNote开源版本的核心功能部分的这个子系统，已经基本完成了，只是这些类还没有被实际创建出实例对象而已
  }
{要创建出类的对象，当然是很容易的。
  只是，由于Sunny SmartNote支持多种编辑器组件的切换（即TssnEditor不同派生类的切换）
  支持多种工作区界面风格的切换（即TssnWorkSpace/TTssnWorkSpaceMgr不同派生类的切换）
  简单第创建这些类的对象是达不到这种灵活性的目的的
  }
{因此，必须另辟蹊径，来寻求这种灵活性。Sunny SmartNote找到了解决方案，这就是使用构造器TssnEditorCtor/TssnWorkSpaceMgrCtor
  }
{需求中，要求编辑器组件是可替换的，多工作区的界面风格也是可替换的
  这就要求如同TssnEditor与TssnWorkSpace/TssnWorkSpaceMgr的派生类实例的创建具有灵活性
  即可以允许程序指定创建何种类型、何种风格的实例对象
  }
{凡是需要灵活性的地方，就需要抽象
  是的，现在是对创建类的实例的逻辑进行抽象的时候了
  这时，就需要“构造器”了
  “构造器”类似一个工厂，只需要告诉它需要什么类型的产品（即对象实例），即可由它为我们生产
  }
{首先，需要为某一种“构造器”定义一个接口规范，即抽象类。
  然后从该抽象类派生出具体生产某一类型产品的派生类
  这样，在需要创建不同的产品时，只要创建出不同的“构造器”实例对象，就可以生产出不同的“产品”
  }

interface

uses stdctrls, controls,
     Editor;

type
    {创建编辑器组件的“构造器”
      这是一个抽象类，它为其所有派生类（具体的构造器）定义一套统一规范
      其派生类对应于每一个TssnEditor的派生类，作为它们的生产厂

      构造器的对外接口很简单，一般只需要一个创建产品实例的方法即可
        所以TssnEditorCtor假如唯一的一个方法：CreateAnEditor
        该方法通过参数var Editor : TssnEditor 传出被创建出来的TssnEditor类型的对象引用
      可以把CreateAnEditor声明为一个抽象虚方法，由具体创建每种风格的编辑器实例的TssnEditorCtor的派生类来实现
        但是，有一些事情需要正在基类中完成
        因此还是使用已经使用过多次的Template Method的做法
        将CreateAnEditor声明为非虚方法，而为其配套假如入一个DoCreateAnEditor的抽象虚方法，由其派生类来实现
      }
    TssnEditorCtor = class
    protected
        function DoCreateAnEditor(var Editor : TssnEditor; ParentCtrl : TWinControl) : Integer; virtual; abstract;
    public
        function CreateAnEditor(var Editor : TssnEditor; ParentCtrl : TWinControl) : Integer;
    end;

    {TssnMemoEditorCtor是TssnMemoEditor的构造器
      }
    TssnMemoEditorCtor = class(TssnEditorCtor)
    protected
        function DoCreateAnEditor(var Editor : TssnEditor; ParentCtrl : TWinControl) : Integer; override;
    end;

    {TssnRichEditorCtor是TssnRichEditor的构造器
      }
    TssnRichEditorCtor = class(TssnEditorCtor)
    protected
        function DoCreateAnEditor(var Editor : TssnEditor; ParentCtrl : TWinControl) : Integer; override;
    end;

implementation

uses MemoEditor, RichEditor, SettingMgr, GlobalObject;

{ tssnMemoEdtitorCtor }

{创建一个TssnMemoEditor实例
  }
function TssnMemoEditorCtor.DoCreateAnEditor(var Editor: TssnEditor; ParentCtrl : TWinControl): Integer;
begin
    Editor := TssnMemoEditor.Create(ParentCtrl);
    Result := Integer(Editor <> nil);
end;

{ TssnEditorCtor }

{在TssnEditorCtor中要实现的，仅仅只有CreateAnEditor
  该方法的算法是调用被其派生类覆盖的抽象虚方法DoCreateAnEditor，然后为创建出来的对象设置一些初始值
  }
function TssnEditorCtor.CreateAnEditor(var Editor: TssnEditor;
  ParentCtrl: TWinControl): Integer;
begin
    {传入ParentCtrl指明编辑器组件所在父窗口组件
      调用派生类的DoCreateAnEditor以创建真正的编辑器组件来实现
      具体的编辑器组件风格决定于使用哪种TssnEditorCtor的派生类实例
      }
    Result := DoCreateAnEditor(Editor, ParentCtrl);

    {创建成功，则将默认字体设置作用于编辑器组件
      }
    if Boolean(Result) then
        Editor.SetFont(g_SettingMgr.GetDefaultFont());
end;

{ TssnRichEditorCtor }

{创建一个TssnRichEditor实例
  }
function TssnRichEditorCtor.DoCreateAnEditor(var Editor: TssnEditor;
  ParentCtrl: TWinControl): Integer;
begin
    Editor := TssnRichEditor.Create(ParentCtrl);
    Result := Integer(Editor <> nil);
end;

end.
