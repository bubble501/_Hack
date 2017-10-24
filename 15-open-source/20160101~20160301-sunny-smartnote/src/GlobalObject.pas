////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   GlobalObject.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit GlobalObject;

interface

uses Forms, SysUtils,
     WorkSpaceMgr, EditorCtor, InterActive, UMainForm, EditorEvent,
     WorkSpaceEvent, SettingMgr;

var
    g_EditorCtor : TssnEditorCtor = nil;
    g_WorkSpaceMgr : TssnWorkSpaceMgr = nil;
    g_InterActive : TssnInterActive = nil;
    g_EditorEvent : TssnEditorEvent = nil;
    g_WorkSpaceEvent : TssnWorkSpaceEvent = nil;
    g_SettingMgr : TssnSettingMgr = nil;

    g_MainForm: TMainForm = nil;


    //�������������ĵ��ÿ��Բμ�snote.dpr�ļ�����Ĵ����߼�
    function InitObjects() : Integer;    //����������Ҫ�Ķ���
    procedure UnInitObjects();           //�������н������ͷŴ����Ķ���

implementation

uses WorkSpaceMgrCtor;

function InitObjects() : Integer;
var
    WorkSpaceMgrCtor : TssnWorkSpaceMgrCtor;   
begin
    g_SettingMgr := TssnSettingMgr.Create(ChangeFileExt(ExtractFileName(Application.ExeName), ''), true);  //�����Ĭ�����ù�����ʵ������

    g_EditorEvent := TssnEditorEvent.Create();                 //�����༭���¼�ί��ʵ��
    g_WorkSpaceEvent := TssnWorkSpaceEvent.Create();           //�����������¼�ί��ʵ��
    Application.CreateForm(TMainForm, g_MainForm);             //������Ŀ������

    WorkSpaceMgrCtor := TssnTabWorkSpaceMgrCtor.Create();      //����������������������ʵ��
    g_EditorCtor := TssnRichEditorCtor.Create();               //�����༭�����������ʵ��
    WOrkSpaceMgrCtor.CreateAWorkSpaceMgr(g_WorkSpaceMgr, g_MainForm.pnl_WorkSpace);    //����������������ʵ��
    WorkSpaceMgrCtor.Free();        //ʹ�ù�����������������ʵ�������깤����������ʵ��֮�󣬾Ϳ��Խ��������������������ͷ���

    g_InterActive := TssnInterActive.Create(g_MainForm.Handle);//�û��������ʵ�������������ǣ���������

    g_MainForm.Init();

    Result := 1;
end;

{�ͷ���InitObjects�����д�����ʵ��}
procedure UnInitObjects();
begin
    g_InterActive.Free();
    g_InterActive := nil;

    g_EditorCtor.Free();
    g_EditorCtor := nil;

    g_WorkSpaceMgr.Free();
    g_WorkSpaceMgr := nil;

    g_WorkSpaceEvent.Free();
    g_WorkSpaceEvent := nil;

    g_EditorEvent.Free();
    g_EditorEvent := nil;

    g_SettingMgr.Free();
    g_SettingMgr := nil;
end;

end.
