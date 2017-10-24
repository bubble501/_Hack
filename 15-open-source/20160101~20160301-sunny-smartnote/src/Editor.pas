////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   Editor.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit Editor;

interface

uses Controls, Graphics,
     IntfEditor;

type
    {TssnEditor是一个抽象类，抽象出该编辑器应用程序需求中所需要的所有编辑器组件的特征---行为和属性
      然后在代码中可以方便的实现编辑器组件的随时替换
      详细看《Delphi高手突破》中的解释说明

    TssnEditor的职责是为所有的编辑器组件进行抽象，并完成它们的公共代码
      所以需要使用的时候还是需要创建继承自它的具体的类
      }
    TssnEditor = class(IssnEditor)
    private
        {TssnEditor作为编辑器组件，编辑器的操作无非两大类：文件访问（读取和保存）和文字编辑
          其中文字编辑一般与具体的编辑器组件密切相关

          先说一下文件访问，文件访问要求在TssnEditor内部保存该组件所对应的文件名称，可以定义一个private的m_FileName为文件名
            在读取文件时设置该成员的值
            在“另存为”时改变该成员的值
            在保存的时候直接将编辑器组件中的文本保存到m_FileName所指的文件
          }
        m_FileName : String;
        
    protected
        {LoadFromFile和DoLoadFromFile
          LoadFromFile的算法是将m_FileName设置为所要打开的文件名，然后打开文件。
          其中为m_FileName设置值是派生类无法做到的，需要在TssnEditor中实现。
          而实际打开文件的动作与具体的编辑器组件有关，必须延迟到具体的派生类中实现。
          这是，virtual就可以帮上忙了。
          可以定义一个非虚方法LoadFromFile并声明抽象虚方法DoLoadFromFile，
            由派生类所实现的DoLoadFromFile实际将文件读取到编辑器组件中，
            而LoadFromFile负责调用DoLoadFromFile并设置m_FileName的值

        设计模式层面的解释
          这种做法也是设计模式的一种，被称为“Template Method”，它的应用非常普遍。
          在VCL中，TObject非虚的Free()方法和虚方法Destroy()之间的关系，就如同LoadFromFile()和DoLoadFromFile()的关系一样    
          }
        procedure DoLoadFromFile(FileName : String); virtual; abstract;

        {该方法与整个软件框架中的时间委托模型相关
          详细见EditorEvent、WorkSpaceEvent这两个单元
          }
        procedure OnEditorSelectionChange(Sender : TObject);

        function GetText() : String; virtual; abstract;

    public
        procedure LoadFromFile(FileName : String);    //详细见DoLoadFromFile方法上面的注释说明

        {SaveToFile、Save和SaveAs
          文件访问，除了读取文件，显然还有保存文件。IssnEditor接口已经给出了两个方法Save()和SaveAs()用于保存功能，在TssnEditor中可以实现这两个方法
          SaveAs()的算法是显示一个“另存为”的对话框，如果指定了正确的文件名，则保存文本到文件
            与LoadFromFile类似，只有真正保存文件的动作才与实际编辑器组件相关，可以另外声明一个SaveToFile的抽象虚方法并由派生类实现具体的保存文本的动作
            而弹出“另存为”对话框则属于公共逻辑，可以在TssnEditor.SaveAs()中实现
            可以发现，这同样是Template Method模式的应用
          Save()的算法是检查m_FileName是否为空字符串，是（未命名文件）则调用SaveAs()，否则调用SaveToFile()保存文本
          }
        procedure SaveToFile(FileName : String); virtual; abstract;
        function Save() : Boolean; override;
        function SaveAs() : Boolean; override;

        {GetText和GetWordCount
          TssnEditor的文件访问功能在上面的打开文件、保存文件的相关方法中都已经实现了（除了应该由派生类做的，如DoLoadFromFile和SaveToFile）
          剩下的文字编辑功能基本上都与具体的编辑器组件有关，因此应由TssnEditor的派生类实现。
          不过，其中的统计字数功能，由于在此准备对所有编辑器采用一致的算法进行字数统计，因此决定将该功能在TssnEditor中实现。
          统计字数的算法是首先获取编辑器中的所有文本（字符串形式），然后逐字符进行统计。
            也许读者也已经想到，取得编辑器中的所有文本字符串需要由具体你的编辑器组件（TssnEditor的派生类）提供，
            而统计步骤则在TssnEditor中实现，这又是一个Template Method模式
            对应抽象虚方法GetText()和非虚方法GetWordCount()
          }
        function GetWordCount() : TssnWordCountRec; override;

        function GetFileName() : String; override;            //获取文件名
    end;

implementation

uses GlobalObject;

{ TssnEditor }

function TssnEditor.GetFileName: String;
begin
    Result := m_FileName;
end;

{ TssnWordCountRec = record
        AnsiChar : Integer;
        MultiChar : Integer;
        NumChar : Integer;
        Other : Integer;
    end;
  }
function TssnEditor.GetWordCount: TssnWordCountRec;
var
    AllText : String;
    bHalf : Boolean;
    i : Integer;
    nAsc : Integer;
begin
    {获取编辑器中的所有文本（字符串）
      GetText()有TssnEditor的各个派生类具体实现
      }
    AllText := GetText();
    bHalf := false;

    {然后逐字符进行统计
      String轮询各个字符的顺序是从 1 开始到 Length(AllText)
      }
    for i := 1 to Length(AllText) do
    begin
        {ord方法作用：Char 类型与其编码值的转换，用于取每个字符的ASCII编码
          详细参见：http://www.xumenger.com/delphi-ord-20160222/
          }
        nAsc := ord(AllText[i]);

        {如果处于双字节字符的第二个字符，则忽略该字符}
        if bHalf then
        begin
            bHalf := false;
            nAsc := 0;
        end;

        if nAsc > 127 then                             //如果当前自己的ASCII代码 >127，则认为其实双字节字符
        begin //chinese
            if not bHalf then
                Inc(Result.MultiChar);
            bHalf := not bHalf;
        end

        else if (nAsc >= 48) and (nAsc <= 57) then     //数字字符，0-9
            Inc(Result.NumChar)

        else if (nAsc >= 65) and (nAsc <= 90) then     //英文字母字符，A-Z
            Inc(Result.AnsiChar)

        else if (nAsc >= 97) and (nAsc <= 122) then    //英文字母字符，a-z
            Inc(Result.AnsiChar)

        else if (nAsc <> 32) and (nAsc <> 0) and (nAsc <> 13) and (nAsc <> 10) then         //其他字符
            Inc(Result.Other);
    end;
end;

procedure TssnEditor.LoadFromFile(FileName: String);
begin
    m_FileName := FileName;

    {实际读取文件到编辑器的动作，延迟到派生类实现
      因此DoLoadFromFile为virtual; abstract;的
      如果FileName = '' ，表示新建一个编辑器组件，不打开任何文件
      }
    if FileName <> '' then
        DoLoadFromFile(FileName);
end;

procedure TssnEditor.OnEditorSelectionChange(Sender: TObject);
begin
    g_EditorEvent.OnEditorSelectionChange(Sender);
end;

function TssnEditor.Save: Boolean;
begin
    {Save()的算法是检查m_FileName是否为空字符串，
      是（未命名文件）则调用SaveAs()，
      否则调用SaveToFile()保存文本
      }
    if m_FileName = '' then
    begin
        Result := SaveAs();
        Exit;
    end;

    SaveToFile(m_Filename);
    Result := true;
end;

function TssnEditor.SaveAs: Boolean;
var
    FileName : String;
begin
    Result := false;
    
    {通过弹出“另存为”的对话框，获取需要保存的文件名
      }
    FileName := g_InterActive.ShowSaveDlg();
    if FileName = '' then
        Exit;
    SaveToFile(FileName);
    m_FileName := FileName;
    Result := true;
end;

end.
