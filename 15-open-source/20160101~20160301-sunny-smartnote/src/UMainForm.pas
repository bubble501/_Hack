unit UMainForm;

{前面所有的代码实现了 Sunny SmartNote 的功能。只是，至今还没有提到过主界面代码。
  现在就来看一下整个程序中最直观、最可视的模块——界面模块。完成了该模块后，程序就处于随时可运行的状态了。

界面模块部分， 主要是配合我们设计的其他功能模块， 将功能以 UI（用户界面） 方式展示给用户。
  这对于习惯于 Delphi 这样的可视化开发工具的用户来说， 应该是非常熟悉的。
  因此，不准备浪费口舌详细介绍界面模块的代码。
  可以如同使用组件一样来使用我们构造的各个子模块，如 TssnWorkSpace、 TssnWorkSpaceMgr、 TssnInterActive 等。
  }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ExtCtrls, StdCtrls, Menus, ImgList, Buttons;

type
  TMainForm = class(TForm)
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    StatusBar: TStatusBar;
    pnl_WorkSpace: TPanel;

    {关于Delphi的按钮的使用是的一些说明，这里面并没有实现这个按钮的OnClick事件，但是为什么将程序编译出来后，点击new这个按钮可以新建一个工作区？
      打开Object Inspector之后，查看tb_new的Events，其中MenuItem的值是memu_new，MemuItem下的OnClick是memu_newClick
      这样就能保证点击tb_new按钮就相当于点击菜单项memu_new
      本项目的其他button控件也都基本是这样处理的}
    tb_new: TToolButton;

    tb_open: TToolButton;
    MainMenu: TMainMenu;
    menu_file: TMenuItem;
    menu_new: TMenuItem;
    menu_open: TMenuItem;
    ImageList: TImageList;
    menu_line0: TMenuItem;
    menu_save: TMenuItem;
    tb_save: TToolButton;
    tb_line1: TToolButton;
    menu_saveall: TMenuItem;
    tb_saveall: TToolButton;
    menu_saveas: TMenuItem;
    menu_line1: TMenuItem;
    menu_close: TMenuItem;
    menu_closeall: TMenuItem;
    menu_line2: TMenuItem;
    menu_exit: TMenuItem;
    menu_edit: TMenuItem;
    menu_undo: TMenuItem;
    menu_redo: TMenuItem;
    menu_line3: TMenuItem;
    menu_cut: TMenuItem;
    menu_copy: TMenuItem;
    menu_paste: TMenuItem;
    menu_del: TMenuItem;
    menu_DeleteSelection: TMenuItem;
    menu_DeleteLine: TMenuItem;
    tb_cut: TToolButton;
    tb_copy: TToolButton;
    tb_paste: TToolButton;
    tb_del: TToolButton;
    menu_line4: TMenuItem;
    menu_selectall: TMenuItem;
    tb_line2: TToolButton;
    tb_find: TToolButton;
    menu_search: TMenuItem;
    menu_find: TMenuItem;
    menu_findnext: TMenuItem;
    menu_line5: TMenuItem;
    menu_replace: TMenuItem;
    menu_tools: TMenuItem;
    menu_WorkSpace: TMenuItem;
    menu_Help: TMenuItem;
    menu_wordcount: TMenuItem;
    menu_line6: TMenuItem;
    menu_setting: TMenuItem;
    menu_nextworkspace: TMenuItem;
    menu_about: TMenuItem;
    menu_line7: TMenuItem;
    menu_wrap: TMenuItem;
    procedure menu_newClick(Sender: TObject);
    procedure menu_openClick(Sender: TObject);
    procedure menu_saveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure menu_saveallClick(Sender: TObject);
    procedure menu_saveasClick(Sender: TObject);
    procedure menu_closeClick(Sender: TObject);
    procedure menu_closeallClick(Sender: TObject);
    procedure menu_exitClick(Sender: TObject);
    procedure menu_undoClick(Sender: TObject);
    procedure menu_redoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure menu_cutClick(Sender: TObject);
    procedure menu_copyClick(Sender: TObject);
    procedure menu_pasteClick(Sender: TObject);
    procedure menu_DeleteSelectionClick(Sender: TObject);
    procedure menu_DeleteLineClick(Sender: TObject);
    procedure menu_selectallClick(Sender: TObject);
    procedure menu_findClick(Sender: TObject);
    procedure menu_findnextClick(Sender: TObject);
    procedure menu_replaceClick(Sender: TObject);
    procedure menu_wordcountClick(Sender: TObject);
    procedure menu_nextworkspaceClick(Sender: TObject);
    procedure menu_aboutClick(Sender: TObject);
    procedure menu_settingClick(Sender: TObject);
    procedure menu_wrapClick(Sender: TObject);
  private
    m_LastFindText : String;
    m_LastFindOption : TFindOptions;

  public
    procedure Init();
    procedure UpdateMenuToolBar_WorkSpace();       //更新工作区相关菜单
    procedure UpdateMenuToolBar_Editor();          //更新编辑器相关菜单，仔细看其内部实现
                                 
    procedure OnEditorChange(Sender : TObject);       //编辑器组件事件委托回调函数
    procedure OnWorkSpaceOpenClose(Sender : TObject); //工作区事件委托回调函数(工作区数量发生变化)
    procedure OnWorkSpaceChange(Sender : TObject);    //工作区事件委托的另一个回调函数(当前工作区被改变)
    procedure OnFind(FindText : String; Options : TFindOptions);
    procedure OnReplace(FindText, ReplaceText : String; Options : TFindOptions);
  end;

implementation

uses GlobalObject, WorkSpace, MultiLan, IntfEditor, ssnPublic;

{$R *.DFM}

{【File】-->【 New】 菜单项负责新建一个工作区， 其只需要调用 TssnWorkSpaceMgr 对象的 NewWorkSpace()方法即可：}
procedure TMainForm.menu_newClick(Sender: TObject);
begin
    g_WorkSpaceMgr.NewWorkSpace('');    //创建一个新的工作区，文件名为''
    UpdateMenuToolBar_WorkSpace();      //更新工作区相关菜单
    UpdateMenuToolBar_Editor();         //更新编辑器相关菜单，仔细看其内部实现
end;

procedure TMainForm.menu_openClick(Sender: TObject);
var
    FileList : TStrings;
    i : Integer;
begin
    FileList := g_InterActive.ShowOpenDlg();
    if FileList = nil then
        Exit;

    for i := 0 to FileList.Count - 1 do
    begin
        try
            g_WorkSpaceMgr.NewWorkSpace(FileList[i]);
        except
            g_InterActive.MessageBox(Format(str_LoadError, [FileList[i]]), Application.Title, MB_ICONSTOP);
        end;
    end;

    UpdateMenuToolBar_WorkSpace();
end;

{更新工作区相关菜单}
procedure TMainForm.UpdateMenuToolBar_WorkSpace;
var
    bEnable : Boolean;
    nWorkSpaceCount : Integer;
begin
    nWorkSpaceCount := g_WorkSpaceMgr.GetWorkSpaceCount();     //获取当前打开的工作区的个数

    if nWorkSpaceCount > 0 then
        bEnable := true
    else
        bEnable := false;

    menu_save.Enabled := bEnable;      //是否可“保存”
    tb_save.Enabled := bEnable;        //是否可“保存”
    menu_saveas.Enabled := bEnable;    //是否可“另存为”
    menu_saveall.Enabled := bEnable;   //是否可“保存全部”
    tb_saveall.Enabled := bEnable;     //是否可“保存全部”
    menu_close.Enabled := bEnable;     //是否可“关闭”
    menu_closeall.Enabled := bEnable;  //是否可“关闭全部”
    menu_undo.Enabled := bEnable;      //是否可“撤销”
    menu_redo.Enabled := bEnable;      //是否可“重做”
    menu_cut.Enabled := bEnable;       //是否可“剪切”
    tb_cut.Enabled := bEnable;         //是否可“剪切”
    menu_copy.Enabled := bEnable;      //是否可“拷贝” 
    tb_copy.Enabled := bEnable;        //是否可“拷贝”
    menu_paste.Enabled := bEnable;     //是否可“粘贴”
    tb_paste.Enabled := bEnable;       //是否可“粘贴”
    menu_del.Enabled := bEnable;       //是否可“删除”
    tb_del.Enabled := bEnable;         //是否可“删除”
    menu_selectall.Enabled := bEnable; //是否可“选择全部”
    menu_find.Enabled := bEnable;      //是否可“查找”
    tb_find.Enabled := bEnable;        //是否可“查找”
    menu_findnext.Enabled := bEnable;  //是否可“查找下一个”
    menu_replace.Enabled := bEnable;   //是否可“替换”
    menu_wordcount.Enabled := bEnable; //是否可“统计字符个数”
    menu_wrap.Enabled := bEnable;      //是否可“自动换行”

    if nWorkSpaceCount > 1 then
        menu_nextworkspace.Enabled := true
    else
        menu_nextworkspace.Enabled := false;
end;

{【File】-->【Save】 菜单项负责保存当前工作区文本。
  它首先获得当前激活的工作区对象， 然后调用其 Save()方法进行保存工作。
  不过， 需要将 Save()方法的调用用 try 块保护起来，因为 Save()方法可能会引发异常，如文件为只读时，保存会失败。
  如果保存失败，则提示用户“文件无法保存”，然后自动转为“另存为”功能
  }
procedure TMainForm.menu_saveClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();    //获取当前工作区
    if CurWorkSpace = nil then
        Exit;
    try
        CurWorkSpace.Save()   //保存
    except
        g_InterActive.MessageBox(str_SaveError, Application.Title, MB_ICONSTOP);
        menu_saveas.Click();
    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if not g_WorkSpaceMgr.CloseAll() then
        Action := caNone;
end;

procedure TMainForm.menu_saveallClick(Sender: TObject);
begin
    g_WorkSpaceMgr.SaveAll();
end;

procedure TMainForm.menu_saveasClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    try
        CurWorkSpace.SaveAs()
    except
        g_InterActive.MessageBox(str_SaveError, Application.Title, MB_ICONSTOP);
    end;
end;

procedure TMainForm.menu_closeClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    g_WorkSpaceMgr.CloseWorkSpace(CurWorkSpace.GetIndex());
end;

procedure TMainForm.menu_closeallClick(Sender: TObject);
begin
    g_WorkSpaceMgr.CloseAll();
end;

procedure TMainForm.menu_exitClick(Sender: TObject);
begin
    Close();
end;

{更新编辑器相关的菜单}
procedure TMainForm.UpdateMenuToolBar_Editor;
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    menu_undo.Enabled := CurWorkSpace.CanUndo();    //menu_undo: TMenuItem;  CurWorkSpace.CanUndo()是获取当前工作区是不是处于“可撤销”状态
    menu_redo.Enabled := CurWorkSpace.CanRedo();    //menu_redo: TMenuItem;  CurWorkSpace.CanRedo()是获取当前工作区是不是处于“重做”状态
    menu_cut.Enabled := CurWorkSpace.CanCut();      //menu_cut: TMenuItem;   CurWorkSpace.CanCut()是获取当前工作区是不是处于“可剪切”状态
    tb_cut.Enabled := menu_cut.Enabled;             //tb_cut: TToolButton;   和memu_cut菜单项状态保持一致
    menu_copy.Enabled := CurWorkSpace.CanCopy();    //menu_copy: TMenuItem;  CurWorkSpace.CanCopy()是获取当前工作区是不是处于“可拷贝”状态
    tb_copy.Enabled := menu_copy.Enabled;           //tb_copy: TToolButton;  和memu_copy菜单项状态保持一致
    menu_paste.Enabled := CurWorkSpace.CanPaste();  //menu_paste: TMenuItem; CurWorkSpace.CanPaste()是获取当前工作区是不是处于“可粘贴”状态
    tb_paste.Enabled := menu_paste.Enabled;         //tb_paste: TToolButton; 和menu_paste菜单项状态保持一致

    {menu_deleteselection: TMenuItem;   CurWorkSpace.CanDeleteSelection()是获取工作区是否处于允许执行“删除所选字符”操作状态}
    menu_deleteselection.Enabled := CurWorkSpace.CanDeleteSelection();
    tb_del.Enabled := menu_deleteselection.Enabled; //tb_del: TToolButton;   和menu_deleteselection菜单项状态保持一致
end;

procedure TMainForm.menu_undoClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.Undo();
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.menu_redoClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.Redo();
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.Init;
begin
    UpdateMenuToolBar_WorkSpace();
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    {界面初始化时，为g_EditorEvent设置回调函数指针}

    {设置编辑器组件事件委托回调函数，参数中的OnEditorChange为界面模块的一个函数指针}
    g_EditorEvent.SetOnEditorSelectionChange(OnEditorChange);

    {设置工作区事件委托回调函数}
    g_WorkSpaceEvent.SetOnWorkSpaceOpenClose(OnWorkSpaceOpenClose);  //工作区数量发生变化

    {设置工作区事件委托的另一个回调函数}
    g_WorkSpaceEvent.SetOnWorkSpaceChange(OnWorkSpaceChange);        //当前工作区被改变
end;

procedure TMainForm.OnEditorChange(Sender: TObject);
begin
    UpdateMenuToolBar_Editor();
end;

{【Edit】-->【Cut】菜单项是对当前工作区中的选中文本进行剪切操作。
  只需先获得当前激活的工作区对象，然后调用其 Cut()方法即可。
  如前几节所说，工作区对象会将 Cut()方法的调用转发给其内部的编辑器组件。
  当然，这属于内部实现细节了，界面层的代码编写不需要知道这些：
  }
procedure TMainForm.menu_cutClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();    //获取当前工作区
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.Cut();   //剪切
    UpdateMenuToolBar_Editor();  //更新编辑器相关的菜单
end;

procedure TMainForm.menu_copyClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.Copy();
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.menu_pasteClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.Paste();
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.menu_DeleteSelectionClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.DeleteSelection();
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.menu_DeleteLineClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.DeleteLine();
    UpdateMenuToolBar_Editor();
end;

//工作区事件委托回调函数(工作区数量发生变化)
procedure TMainForm.OnWorkSpaceOpenClose(Sender: TObject);
begin
    UpdateMenuToolBar_WorkSpace();        {更新工作区相关菜单}
end;

procedure TMainForm.menu_selectallClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.SelectAll();
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.menu_findClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    g_InterActive.ShowFindDlg(CurWorkSpace.GetSelectText(), OnFind);
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.OnFind(FindText: String; Options: TFindOptions);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    m_LastFindText := FindText;
    m_LastFindOption := Options;
    if not CurWorkSpace.FindNext(FindText, Options) then
        g_InterActive.MessageBox(Format(str_NotFindText, [FindText]), Application.Title, MB_ICONINFORMATION);
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.menu_findnextClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    if not CurWorkSpace.FindNext(m_LastFindText, m_LastFindOption) then
        g_InterActive.MessageBox(Format(str_NotFindText, [m_LastFindText]), Application.Title, MB_ICONINFORMATION);
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.OnReplace(FindText, ReplaceText: String;
  Options: TFindOptions);
var
    CurWorkSpace : TssnWorkSpace;
    ReplaceCount : Integer;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    ReplaceCount := CurWorkSpace.Replace(FindText, ReplaceText, Options);
    if frReplaceAll in Options then
        g_InterActive.MessageBox(Format(str_ReplacedAll, [ReplaceCount]), Application.Title, MB_ICONINFORMATION)
    else if not CurWorkSpace.FindNext(FindText, Options) then
        g_InterActive.MessageBox(Format(str_NotFindText, [m_LastFindText]), Application.Title, MB_ICONINFORMATION);
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.menu_replaceClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    g_InterActive.ShowReplaceDlg(CurWorkSpace.GetSelectText(), OnFind, OnReplace);
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.menu_wordcountClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
    CountResult : TssnWordCountRec;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    CountResult := CurWorkSpace.GetWordCount();
    g_InterActive.MessageBox(
        String(str_CountResult) +
        SSN_ENTER_CHAR + SSN_ENTER_CHAR +
        String(str_AnsiChar) + IntToStr(CountResult.AnsiChar) + SSN_ENTER_CHAR +
        String(str_MultiChar) + IntToStr(CountResult.MultiChar) + SSN_ENTER_CHAR +
        String(str_NumChar) + IntToStr(CountResult.NumChar) + SSN_ENTER_CHAR +
        String(str_OtherChar) + IntToStr(CountResult.Other),
        Application.Title,
        MB_ICONINFORMATION
    );
    UpdateMenuToolBar_Editor();
end;

procedure TMainForm.menu_nextworkspaceClick(Sender: TObject);
begin
    g_WorkSpaceMgr.ActiveNextWorkSpace();
end;

procedure TMainForm.menu_aboutClick(Sender: TObject);
begin
    g_InterActive.MessageBox(
        'Sunny SmartNote 5.0 (OpenSource Edition)' + SSN_ENTER_CHAR + SSN_ENTER_CHAR +
        'build 2002.5.17' + SSN_ENTER_CHAR + SSN_ENTER_CHAR +
        'Author : Shen Min' + SSN_ENTER_CHAR +
        'Copyright(c) 1999-2002 by Sunisoft' + SSN_ENTER_CHAR +
        'http://www.sunisoft.com',
        Application.Title,
        MB_ICONINFORMATION
    );
end;

procedure TMainForm.menu_settingClick(Sender: TObject);
var
    Font : TFont;
begin
    Font := g_InterActive.ShowFontDlg();
    if Font <> nil then
        g_SettingMgr.SetDefaultFont(Font);
end;

procedure TMainForm.menu_wrapClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.SetWordWrap(menu_wrap.Checked);
    UpdateMenuToolBar_Editor();
end;

//工作区事件委托的另一个回调函数(当前工作区被改变)
procedure TMainForm.OnWorkSpaceChange(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();     //因为当前工作区被改变，所以需要获取新的被激活的工作区
    if CurWorkSpace = nil then
        Exit;
    menu_wrap.Checked := CurWorkSpace.GetWordWrap();         //新的工作区是否可换行

    {将新的工作区的“文件名”设置为StatusBar.SimpleText
      StatusBar就是编辑器最下面的那个状态栏
      实际的运行效果是：将当前工作区的 文件名（包括路径）显示在编辑器下面的StatusBar中}
    StatusBar.SimpleText := CurWorkSpace.GetFileName();
end;

end.
