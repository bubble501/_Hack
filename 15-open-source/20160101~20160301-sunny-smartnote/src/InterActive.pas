////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   InterActive.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-5
//  Comment     :   Default use Win32 API
//
//
////////////////////////////////////////////////////////////////////////////////

unit InterActive;

{应用程序少不了要和用户交互，于是程序中会使用大量的各种对话框。
  Windows 系统API 也为用户提供了标准的必备的对话框。
  一般情况下， 系统提供的标准对话框已经可以满足用户的需要。
  但有时随着应用程序版本的升级，可能会不满足于使用标准对话框，
  或者软件界面风格发生变化，如果希望所有对话框风格能够与软件主界面一起变化以使得整体界面更为协调、统一，
  那么就可能需要重写对话框部分相关的代码。
  }
{如何降低这种需求的变化给代码带来的影响呢？是的，答案还是——抽象！}
{于是， 又抽象出一个对话框的逻辑的类——TssnInterActive。
  它负责定义一套对话框使用规范， 对外提供各种对话框（ 打开文件、 文件另存为、 查找、替换等），
  而具体每种风格的对话框由其派生类实现。
  当然，可以在 TssnInterActive 中给出使用 Windows 标准对话框的默认实现，
  这样当应用程序使用 Windows 标准对话框时（大多数情况下） ， 就不必为其写派生类而可以直接使用 TssnInterActive 了。}
{为每种对话框提供一个方法以供客户程序调用——
  MessageBox（消息框）、ShowFontDlg（字体选择对话框）、ShowSaveDlg（保存文件对话框）、
  ShowOpenDlg（打开文件对话框）、ShowFindDlg（查找对话框）、ShowReplaceDlg（ 替换对话框）。}

interface

uses Windows, Dialogs, Graphics, Classes;

type
    TssnOnFindEvent = procedure (FindText : String; Options : TFindOptions) of Object;
    TssnOnReplaceEvent = procedure (FindText, ReplaceText : String; Options : TFindOptions) of Object;

    TssnInterActive = class
    private
        {由于在显示某些对话框时， 可能需要指定其父窗口的句柄（如 MessageBox），
          因此在构造该类的实例时，由外部传入一个主窗口的句柄并保存在一个数据成员中。
          另外，TssnInterActive 还有几个数据成员， 它们分别是对应于每种对话框的 VCL 库中的组件类实例，
          如 TSaveDialog、 TOpenDialog、 TFontDialog、 TFindDialog、 TreplaceDialog。
          使用这些组件来实现 TssnInterActive。
          }
        m_hWnd : HWND;           //构造函数传入的主窗体句柄
        m_SaveDlg : TSaveDialog; //“保存文件”对话框
        m_FontDlg : TFontDialog; //“字体选择”对话框
        m_OpenDlg : TOpenDialog; //“打开文件”对话框

        {在这些对话框中， TFindDialog 与 TReplaceDialog 比较特殊， 它们是非模态的， 其流程和事件有关。
          例如， TFindDialog 的对话框显示后， 主线程并不阻塞仍可以继续工作， 直到用户单击了“查找下一个” 按钮触发 OnFind 事件或者单击“取消” 按钮而关闭对话框。
          因此，需要为 TFindDialog 与 TReplaceDialog 提供一套回调机制。
          可以在 TssnInterActive 内部设置两个函数指针类型的数据成员，
          在客户程序调用 ShowFindDlg 或 ShowReplaceDlg时， 传入回调函数指针并保存在这两个数据成员中，
          在对话框组件的 OnFind 与 OnReplace等事件被触发时，调用客户程序的回调函数即可。

          理解什么是模式窗体、什么是非模式窗体，以及主线程在这两种窗体处理时的阻塞情况
          }
        m_FindDlg : TFindDialog; //“查找”对话框
        m_ReplaceDlg : TReplaceDialog;  //“替换”对话框
        m_OnFindCallBack : TssnOnFindEvent;       //“查找”的回调函数指针
        m_OnReplaceCallBack : TssnOnReplaceEvent; //“替换”的回调函数指针
        procedure OnFind(Sender : TObject);
        procedure OnReplace(Sender : TObject);
        procedure OnReplaceFind(Sender : TObject);

    public
        constructor Create(MainWnd : HWND);
        destructor Destroy(); override;

        {其余的显示对话框方法，只是简单地调用对话框对象（如 TFontDialog 的实例）的Execute()方法并将用户选择结果通过返回值返回而已。}
        function MessageBox(Text, Caption : String; uType : Cardinal) : Integer; virtual;
        function ShowSaveDlg() : string; virtual;
        function ShowFontDlg() : TFont; virtual;
        function ShowOpenDlg() : TStrings; virtual;
        procedure ShowFindDlg(defFindText : String; pfOnFind : TssnOnFindEvent); virtual;
        procedure ShowReplaceDlg(defFindText : String; pfOnFind : TssnOnFindEvent; pfOnReplace : TssnOnReplaceEvent); virtual;
    end;

implementation


{ TssnInterActive }

{TssnInterActive 会在构造函数中创建出所有对话框的实例并对它们进行初始化。
  以后每当客户程序需要显示对话框时，由 TssnInterActive 的实例调用这些对话框对象的 Execute()方法。
  当然，这种算法也许显得有些浪费资源（对话框对象在还没有使用时就被创建出来了）。
  当感到这种算法不佳时，可以随时更改 TssnInterActive 的实现， 而不会影响整个程序的其他模块。
  不过，在此还是用这种比较浪费资源的算法来实现。
}
constructor TssnInterActive.Create(MainWnd: HWND);
begin
    m_OnFindCallBack := nil;
    m_hWnd := MainWnd;        //主窗体句柄

    {初始化对话框实例}
    m_SaveDlg := TSaveDialog.Create(nil);
    m_SaveDlg.Options := [ofOverwritePrompt, ofHideReadOnly];

    m_FontDlg := TFontDialog.Create(nil);

    m_OpenDlg := TOpenDialog.Create(nil);
    m_OpenDlg.Options := [ofAllowMultiSelect, ofFileMustExist];

    m_FindDlg := TFindDialog.Create(nil);
    m_FindDlg.OnFind := OnFind;     //设置事件函数

    m_ReplaceDlg := TReplaceDialog.Create(nil);
    m_ReplaceDlg.OnFind := OnReplaceFind; //设置事件函数
    m_ReplaceDlg.OnReplace := OnReplace;  //设置事件函数
end;

destructor TssnInterActive.Destroy;
begin
    m_ReplaceDlg.Free();
    m_ReplaceDlg := nil;

    m_FindDlg.Free();
    m_FindDlg := nil;

    m_OpenDlg.Free();
    m_OpenDlg := nil;

    m_FontDlg.Free();
    m_FontDlg := nil;

    m_SaveDlg.Free();
    m_SaveDlg := nil;

    m_hWnd := 0;
end;

function TssnInterActive.MessageBox(Text, Caption: String;
  uType: Cardinal): Integer;
begin
    Result := Windows.MessageBox(m_hWnd, PChar(Text), PChar(Caption), uType);
end;

{在此，需要再提一下查找/替换对话框的 OnFind 和 OnReplace 事件处理。
  TFindDialog和 TReplaceDialog 是非模态的对话框，打开它们后， 会阻塞主线程，
  直至用户单击了“查找”、“替换”之类的按钮之后，才会调用它们的 OnFind、 OnReplace 之类的事件。
可以看到，在构造函数中有类似这样的代码：m_FindDlg.OnFind := OnFind;
  此时， 就已经将 TFindDialog 对象的 OnFind 事件指针指向了 TssnInterActive 内部的一个 OnFind 方法。
  也就是说， TFindDialog 对象的 OnFind 事件被接管了下来， 可以在 OnFind方法中调用外部模块传进的“查找”事件的回调函数指针
  }
procedure TssnInterActive.OnFind(Sender: TObject);
begin
    m_FindDlg.CloseDialog();
    if Assigned(m_OnFindCallBack) then        //调用客户程序的回调函数
        m_OnFindCallBack(m_FindDlg.FindText, m_FindDlg.Options);
end;

procedure TssnInterActive.OnReplace(Sender: TObject);
begin
    if Assigned(m_OnReplaceCallBack) then
        m_OnReplaceCallBack(m_ReplaceDlg.FindText, m_ReplaceDlg.ReplaceText, m_ReplaceDlg.Options);
end;

procedure TssnInterActive.OnReplaceFind(Sender: TObject);
begin
    if Assigned(m_OnFindCallBack) then
        m_OnFindCallBack(m_ReplaceDlg.FindText, m_ReplaceDlg.Options);
end;

procedure TssnInterActive.ShowFindDlg(defFindText : String; pfOnFind : TssnOnFindEvent);
begin
    m_OnFindCallBack := pfOnFind;
    m_FindDlg.FindText := defFindText;
    m_FindDlg.Execute();
end;

function TssnInterActive.ShowFontDlg: TFont;
begin
    Result := nil;
    if m_FontDlg.Execute() then
        Result := m_FontDlg.Font;
end;

function TssnInterActive.ShowOpenDlg: TStrings;
begin
    Result := nil;
    if m_OpenDlg.Execute() then
        Result := m_OpenDlg.Files;
end;

procedure TssnInterActive.ShowReplaceDlg(defFindText: String;
  pfOnFind: TssnOnFindEvent; pfOnReplace: TssnOnReplaceEvent);
begin
    m_OnReplaceCallBack := pfOnReplace;
    m_OnFindCallBack := pfOnFind;
    m_ReplaceDlg.Execute();
end;

function TssnInterActive.ShowSaveDlg: string;
begin
    Result := '';

    if m_SaveDlg.Execute() then
        Result := m_SaveDlg.FileName;
end;

end.
