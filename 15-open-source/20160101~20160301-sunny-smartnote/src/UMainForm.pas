unit UMainForm;

{ǰ�����еĴ���ʵ���� Sunny SmartNote �Ĺ��ܡ�ֻ�ǣ�����û���ᵽ����������롣
  ���ھ�����һ��������������ֱ�ۡ�����ӵ�ģ�顪������ģ�顣����˸�ģ��󣬳���ʹ�����ʱ�����е�״̬�ˡ�

����ģ�鲿�֣� ��Ҫ�����������Ƶ���������ģ�飬 �������� UI���û����棩 ��ʽչʾ���û���
  �����ϰ���� Delphi �����Ŀ��ӻ��������ߵ��û���˵�� Ӧ���Ƿǳ���Ϥ�ġ�
  ��ˣ���׼���˷ѿ�����ϸ���ܽ���ģ��Ĵ��롣
  ������ͬʹ�����һ����ʹ�����ǹ���ĸ�����ģ�飬�� TssnWorkSpace�� TssnWorkSpaceMgr�� TssnInterActive �ȡ�
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

    {����Delphi�İ�ť��ʹ���ǵ�һЩ˵���������沢û��ʵ�������ť��OnClick�¼�������Ϊʲô�������������󣬵��new�����ť�����½�һ����������
      ��Object Inspector֮�󣬲鿴tb_new��Events������MenuItem��ֵ��memu_new��MemuItem�µ�OnClick��memu_newClick
      �������ܱ�֤���tb_new��ť���൱�ڵ���˵���memu_new
      ����Ŀ������button�ؼ�Ҳ�����������������}
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
    procedure UpdateMenuToolBar_WorkSpace();       //���¹�������ز˵�
    procedure UpdateMenuToolBar_Editor();          //���±༭����ز˵�����ϸ�����ڲ�ʵ��
                                 
    procedure OnEditorChange(Sender : TObject);       //�༭������¼�ί�лص�����
    procedure OnWorkSpaceOpenClose(Sender : TObject); //�������¼�ί�лص�����(���������������仯)
    procedure OnWorkSpaceChange(Sender : TObject);    //�������¼�ί�е���һ���ص�����(��ǰ���������ı�)
    procedure OnFind(FindText : String; Options : TFindOptions);
    procedure OnReplace(FindText, ReplaceText : String; Options : TFindOptions);
  end;

implementation

uses GlobalObject, WorkSpace, MultiLan, IntfEditor, ssnPublic;

{$R *.DFM}

{��File��-->�� New�� �˵�����½�һ���������� ��ֻ��Ҫ���� TssnWorkSpaceMgr ����� NewWorkSpace()�������ɣ�}
procedure TMainForm.menu_newClick(Sender: TObject);
begin
    g_WorkSpaceMgr.NewWorkSpace('');    //����һ���µĹ��������ļ���Ϊ''
    UpdateMenuToolBar_WorkSpace();      //���¹�������ز˵�
    UpdateMenuToolBar_Editor();         //���±༭����ز˵�����ϸ�����ڲ�ʵ��
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

{���¹�������ز˵�}
procedure TMainForm.UpdateMenuToolBar_WorkSpace;
var
    bEnable : Boolean;
    nWorkSpaceCount : Integer;
begin
    nWorkSpaceCount := g_WorkSpaceMgr.GetWorkSpaceCount();     //��ȡ��ǰ�򿪵Ĺ������ĸ���

    if nWorkSpaceCount > 0 then
        bEnable := true
    else
        bEnable := false;

    menu_save.Enabled := bEnable;      //�Ƿ�ɡ����桱
    tb_save.Enabled := bEnable;        //�Ƿ�ɡ����桱
    menu_saveas.Enabled := bEnable;    //�Ƿ�ɡ����Ϊ��
    menu_saveall.Enabled := bEnable;   //�Ƿ�ɡ�����ȫ����
    tb_saveall.Enabled := bEnable;     //�Ƿ�ɡ�����ȫ����
    menu_close.Enabled := bEnable;     //�Ƿ�ɡ��رա�
    menu_closeall.Enabled := bEnable;  //�Ƿ�ɡ��ر�ȫ����
    menu_undo.Enabled := bEnable;      //�Ƿ�ɡ�������
    menu_redo.Enabled := bEnable;      //�Ƿ�ɡ�������
    menu_cut.Enabled := bEnable;       //�Ƿ�ɡ����С�
    tb_cut.Enabled := bEnable;         //�Ƿ�ɡ����С�
    menu_copy.Enabled := bEnable;      //�Ƿ�ɡ������� 
    tb_copy.Enabled := bEnable;        //�Ƿ�ɡ�������
    menu_paste.Enabled := bEnable;     //�Ƿ�ɡ�ճ����
    tb_paste.Enabled := bEnable;       //�Ƿ�ɡ�ճ����
    menu_del.Enabled := bEnable;       //�Ƿ�ɡ�ɾ����
    tb_del.Enabled := bEnable;         //�Ƿ�ɡ�ɾ����
    menu_selectall.Enabled := bEnable; //�Ƿ�ɡ�ѡ��ȫ����
    menu_find.Enabled := bEnable;      //�Ƿ�ɡ����ҡ�
    tb_find.Enabled := bEnable;        //�Ƿ�ɡ����ҡ�
    menu_findnext.Enabled := bEnable;  //�Ƿ�ɡ�������һ����
    menu_replace.Enabled := bEnable;   //�Ƿ�ɡ��滻��
    menu_wordcount.Enabled := bEnable; //�Ƿ�ɡ�ͳ���ַ�������
    menu_wrap.Enabled := bEnable;      //�Ƿ�ɡ��Զ����С�

    if nWorkSpaceCount > 1 then
        menu_nextworkspace.Enabled := true
    else
        menu_nextworkspace.Enabled := false;
end;

{��File��-->��Save�� �˵���𱣴浱ǰ�������ı���
  �����Ȼ�õ�ǰ����Ĺ��������� Ȼ������� Save()�������б��湤����
  ������ ��Ҫ�� Save()�����ĵ����� try �鱣����������Ϊ Save()�������ܻ������쳣�����ļ�Ϊֻ��ʱ�������ʧ�ܡ�
  �������ʧ�ܣ�����ʾ�û����ļ��޷����桱��Ȼ���Զ�תΪ�����Ϊ������
  }
procedure TMainForm.menu_saveClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();    //��ȡ��ǰ������
    if CurWorkSpace = nil then
        Exit;
    try
        CurWorkSpace.Save()   //����
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

{���±༭����صĲ˵�}
procedure TMainForm.UpdateMenuToolBar_Editor;
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();
    if CurWorkSpace = nil then
        Exit;
    menu_undo.Enabled := CurWorkSpace.CanUndo();    //menu_undo: TMenuItem;  CurWorkSpace.CanUndo()�ǻ�ȡ��ǰ�������ǲ��Ǵ��ڡ��ɳ�����״̬
    menu_redo.Enabled := CurWorkSpace.CanRedo();    //menu_redo: TMenuItem;  CurWorkSpace.CanRedo()�ǻ�ȡ��ǰ�������ǲ��Ǵ��ڡ�������״̬
    menu_cut.Enabled := CurWorkSpace.CanCut();      //menu_cut: TMenuItem;   CurWorkSpace.CanCut()�ǻ�ȡ��ǰ�������ǲ��Ǵ��ڡ��ɼ��С�״̬
    tb_cut.Enabled := menu_cut.Enabled;             //tb_cut: TToolButton;   ��memu_cut�˵���״̬����һ��
    menu_copy.Enabled := CurWorkSpace.CanCopy();    //menu_copy: TMenuItem;  CurWorkSpace.CanCopy()�ǻ�ȡ��ǰ�������ǲ��Ǵ��ڡ��ɿ�����״̬
    tb_copy.Enabled := menu_copy.Enabled;           //tb_copy: TToolButton;  ��memu_copy�˵���״̬����һ��
    menu_paste.Enabled := CurWorkSpace.CanPaste();  //menu_paste: TMenuItem; CurWorkSpace.CanPaste()�ǻ�ȡ��ǰ�������ǲ��Ǵ��ڡ���ճ����״̬
    tb_paste.Enabled := menu_paste.Enabled;         //tb_paste: TToolButton; ��menu_paste�˵���״̬����һ��

    {menu_deleteselection: TMenuItem;   CurWorkSpace.CanDeleteSelection()�ǻ�ȡ�������Ƿ�������ִ�С�ɾ����ѡ�ַ�������״̬}
    menu_deleteselection.Enabled := CurWorkSpace.CanDeleteSelection();
    tb_del.Enabled := menu_deleteselection.Enabled; //tb_del: TToolButton;   ��menu_deleteselection�˵���״̬����һ��
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
    {�����ʼ��ʱ��Ϊg_EditorEvent���ûص�����ָ��}

    {���ñ༭������¼�ί�лص������������е�OnEditorChangeΪ����ģ���һ������ָ��}
    g_EditorEvent.SetOnEditorSelectionChange(OnEditorChange);

    {���ù������¼�ί�лص�����}
    g_WorkSpaceEvent.SetOnWorkSpaceOpenClose(OnWorkSpaceOpenClose);  //���������������仯

    {���ù������¼�ί�е���һ���ص�����}
    g_WorkSpaceEvent.SetOnWorkSpaceChange(OnWorkSpaceChange);        //��ǰ���������ı�
end;

procedure TMainForm.OnEditorChange(Sender: TObject);
begin
    UpdateMenuToolBar_Editor();
end;

{��Edit��-->��Cut���˵����ǶԵ�ǰ�������е�ѡ���ı����м��в�����
  ֻ���Ȼ�õ�ǰ����Ĺ���������Ȼ������� Cut()�������ɡ�
  ��ǰ������˵������������Ὣ Cut()�����ĵ���ת�������ڲ��ı༭�������
  ��Ȼ���������ڲ�ʵ��ϸ���ˣ������Ĵ����д����Ҫ֪����Щ��
  }
procedure TMainForm.menu_cutClick(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();    //��ȡ��ǰ������
    if CurWorkSpace = nil then
        Exit;
    CurWorkSpace.Cut();   //����
    UpdateMenuToolBar_Editor();  //���±༭����صĲ˵�
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

//�������¼�ί�лص�����(���������������仯)
procedure TMainForm.OnWorkSpaceOpenClose(Sender: TObject);
begin
    UpdateMenuToolBar_WorkSpace();        {���¹�������ز˵�}
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

//�������¼�ί�е���һ���ص�����(��ǰ���������ı�)
procedure TMainForm.OnWorkSpaceChange(Sender: TObject);
var
    CurWorkSpace : TssnWorkSpace;
begin
    CurWorkSpace := g_WorkSpaceMgr.GetActiveWorkSpace();     //��Ϊ��ǰ���������ı䣬������Ҫ��ȡ�µı�����Ĺ�����
    if CurWorkSpace = nil then
        Exit;
    menu_wrap.Checked := CurWorkSpace.GetWordWrap();         //�µĹ������Ƿ�ɻ���

    {���µĹ������ġ��ļ���������ΪStatusBar.SimpleText
      StatusBar���Ǳ༭����������Ǹ�״̬��
      ʵ�ʵ�����Ч���ǣ�����ǰ�������� �ļ���������·������ʾ�ڱ༭�������StatusBar��}
    StatusBar.SimpleText := CurWorkSpace.GetFileName();
end;

end.
