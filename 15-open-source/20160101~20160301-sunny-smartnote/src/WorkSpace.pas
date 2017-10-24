////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   WorkSpace.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit WorkSpace;

interface

uses Controls, Forms, Windows, Graphics, Dialogs,
     Editor, IntfEditor;

type
    {TssnEditor所表示的编辑器组件并没有出现在客户逻辑中，也就是说客户端只知道“工作区（用TssnWorkSpace封装）”的概念，
      而并不知道有所谓的“编辑器组件”存在。比如，保存一个文件的客户逻辑是：向“工作区”发出“保存”的命令，而不是向
      “编辑器组件”。但是实际程序的实现上，是由“编辑器组件”去执行“保存”命令。
    因此，TssnEditor的实例需要一个符合客户逻辑的容器作为代理，而“工作区”正是扮演这个“代理”的角色
      }

    {TssnEditor完成的是对编辑器组件的抽象，TssnWorkSpace则是对“工作区”概念的抽象
      在逻辑上，它是TssnEditor的容器
      在实现上，它是TssnEditor的代理
        由于TssnWorkSpace也是从IssnEditor接口派生，因此它（或者它的派生类）必须实现IssnEditor所声明的所有方法
        不过，对于这些方法，TssnWorkSpace只需要履行作为代理者的职责即可
          即简单的将请求转发给内部的m_Editor（TssnEditor的实例）
          因为它们都是派生自同一个接口IssnEditor
      }
    {还得考虑一个问题，就是工作区被关闭的问题。关闭意味着销毁其中的m_Editor实例，然后销毁自己，这似乎在析构函数中就可以完成了
      但是可以设想，用户选择关闭工作区时，如果工作区的文本尚未被保存，系统一般会提示询问用户是否保存该文本
        同时也允许选择“取消”，即取消关闭操作
      此时，m_Editor将不能被销毁，工作区自己也不能被销毁
      而如果把关闭的逻辑放到析构函数中，则将无法“后悔”
      解决这个问题很简单，可以提供一个Close方法，以完成判断是否保存、询问用户是否保存等逻辑
      }
    TssnWorkSpace = class(IssnEditor)
    protected
        {由于TssnWorkSpace是TssnEditor的容器，两者是包含和被包含的关系
          因此TssnWorkSpace一定会有一个TssnEditor实例的成员
          }
        m_Editor : TssnEditor;

        {由于有多个工作区存在，必须为每个工作区提供一个标识符（编号）
          这也便于工作区更上层的管理器进行管理工作
          }
        m_Index : Integer;

        procedure OnSave(); virtual; abstract;    //所以TssnWorkSpace是一个抽象类

    public
        {工作区需要实际的载体组件，而其载体组件随着工作区的不同风格的实现也不相同
          因此需要在构造函数中传入一个TWinControl类型的组件引用，以指定工作区具体的载体，将其作为工作区的“父窗体”
          }
        constructor Create(ParentCtrl : TWinControl; FileName : String; nIndex : Integer);

        function Close() : Integer;     //用Close代替Destroy函数
        function GetIndex() : Integer;  //获取工作区标志

        {实现IssnEditor所声明的方法
          对于这些方法，只需要履行代理者的任务，直接将功能调用请求全部转发给m_Editor即可
          所谓转发，就是直接调用m_Editor的相关方法即可
          }
        function GetFileName() : String; override;
        function GetSaved() : Boolean; override;
        function Save() : Boolean; override;
        function SaveAs() : Boolean; override;
        function GetSelectText() : String; override;
        procedure SetFont(Font : TFont); override;
        procedure Undo(); override;
        function CanUndo() : Boolean; override;
        procedure Redo(); override;
        function CanRedo() : Boolean; override;
        procedure Cut(); override;
        function CanCut() : Boolean; override;
        procedure Copy(); override;
        function CanCopy() : Boolean; override;
        procedure Paste(); override;
        function CanPaste() : Boolean; override;
        procedure DeleteSelection(); override;
        function CanDeleteSelection() : Boolean; override;
        procedure DeleteLine(); override;
        procedure SelectAll(); override;
        function FindNext(Text : String; Option : TFindOptions) : Boolean; override;
        function Replace(FindText, ReplaceText : String; Option : TFindOptions) : Integer; override;
        function GetWordCount() : TssnWordCountRec; override;
        function GetWordWrap() : Boolean; override;
        procedure SetWordWrap(WordWrap : Boolean); override;
    end;

implementation

uses GlobalObject, MultiLan;

{ TssnWorkSpace }

function TssnWorkSpace.CanCopy: Boolean;
begin
    Result := m_Editor.CanCopy();
end;

function TssnWorkSpace.CanCut: Boolean;
begin
    Result := m_Editor.CanCut();
end;

function TssnWorkSpace.CanDeleteSelection: Boolean;
begin
    Result := m_Editor.CanDeleteSelection();
end;

function TssnWorkSpace.CanPaste: Boolean;
begin
    Result := m_Editor.CanPaste();
end;

function TssnWorkSpace.CanRedo: Boolean;
begin
    Result := m_Editor.CanRedo();
end;

function TssnWorkSpace.CanUndo: Boolean;
begin
    Result := m_Editor.CanUndo();
end;

function TssnWorkSpace.Close: Integer;
var
    AskRusult : Integer;
begin
    Result := 0;

    if not m_Editor.GetSaved() then //如果文件尚未被保存，则询问用户是否保存并允许“取消”
    begin
        AskRusult := g_InterActive.MessageBox(str_PromptSave, Application.Title, MB_YESNOCANCEL or MB_ICONQUESTION);

        if AskRusult = IDYES then   //保存文本并关闭
        begin // save
            try
                if not m_Editor.Save() then
                    Exit;
            except
                g_InterActive.MessageBox(str_SaveError, Application.Title, MB_ICONSTOP);
                Exit;
            end;
        end
        else if AskRusult = IDCANCEL then   //取消
            Exit;
    end;

    //销毁工作区组件
    m_Editor.Free();
    m_Editor := nil;

    Result := 1;
end;

procedure TssnWorkSpace.Copy;
begin
    m_Editor.Copy();
end;

{工作区需要实际的载体组件，而其载体组件随着工作区的不同风格的实现也不相同
  因此需要在构造函数中传入一个TWinControl类型的组件引用，以指定工作区具体的载体，将其作为工作区的“父窗体”
  }
constructor TssnWorkSpace.Create(ParentCtrl : TWinControl; FileName : String; nIndex : Integer);
begin
    {通过构造器创建m_Editor实例
      关于“构造器”可以参见EditorCtor等单元的代码及我的注释
      }
    g_EditorCtor.CreateAnEditor(m_Editor, ParentCtrl);

    if FileName <> '' then
        m_Editor.LoadFromFile(FileName);

    m_Index := nIndex;
end;

procedure TssnWorkSpace.Cut;
begin
    m_Editor.Cut();
end;

procedure TssnWorkSpace.DeleteLine;
begin
    m_Editor.DeleteLine();
end;

procedure TssnWorkSpace.DeleteSelection;
begin
    m_Editor.DeleteSelection();
end;

function TssnWorkSpace.FindNext(Text: String; Option: TFindOptions) : Boolean;
begin
    Result := m_Editor.FindNext(Text, Option);
end;

function TssnWorkSpace.GetFileName: String;
begin
    Result := m_Editor.GetFileName();
end;

function TssnWorkSpace.GetIndex: Integer;
begin
    Result := m_Index;
end;

function TssnWorkSpace.GetSaved: Boolean;
begin
    Result := m_Editor.GetSaved();
end;

function TssnWorkSpace.GetSelectText: String;
begin
    Result := m_Editor.GetSelectText();
end;

function TssnWorkSpace.GetWordCount: TssnWordCountRec;
begin
    Result := m_Editor.GetWordCount();
end;

function TssnWorkSpace.GetWordWrap: Boolean;
begin
    Result := m_Editor.GetWordWrap();
end;

procedure TssnWorkSpace.Paste;
begin
    m_Editor.Paste();
end;

procedure TssnWorkSpace.Redo;
begin
    m_Editor.Redo();
end;

function TssnWorkSpace.Replace(FindText, ReplaceText: String;
  Option: TFindOptions): Integer;
begin
    Result := m_Editor.Replace(FindText, ReplaceText, Option);
end;

function TssnWorkSpace.Save : Boolean;
begin
    Result := m_Editor.Save();
    OnSave();
end;

function TssnWorkSpace.SaveAs: Boolean;
begin
    Result := m_Editor.SaveAs();
    OnSave();
end;

procedure TssnWorkSpace.SelectAll;
begin
    m_Editor.SelectAll();
end;

procedure TssnWorkSpace.SetFont(Font: TFont);
begin
    m_Editor.SetFont(Font);
end;

procedure TssnWorkSpace.SetWordWrap(WordWrap: Boolean);
begin
    m_Editor.SetWordWrap(WordWrap);
end;

procedure TssnWorkSpace.Undo;
begin
    m_Editor.Undo();
end;

end.
