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


    //下面两个函数的调用可以参见snote.dpr文件里面的代码逻辑
    function InitObjects() : Integer;    //创建任务需要的对象
    procedure UnInitObjects();           //程序运行结束，释放创建的对象

implementation

uses WorkSpaceMgrCtor;

function InitObjects() : Integer;
var
    WorkSpaceMgrCtor : TssnWorkSpaceMgrCtor;   
begin
    g_SettingMgr := TssnSettingMgr.Create(ChangeFileExt(ExtractFileName(Application.ExeName), ''), true);  //程序的默认设置管理类实例创建

    g_EditorEvent := TssnEditorEvent.Create();                 //创建编辑器事件委托实例
    g_WorkSpaceEvent := TssnWorkSpaceEvent.Create();           //创建工作区事件委托实例
    Application.CreateForm(TMainForm, g_MainForm);             //创建项目主窗体

    WorkSpaceMgrCtor := TssnTabWorkSpaceMgrCtor.Create();      //创建工作区管理器构造器实例
    g_EditorCtor := TssnRichEditorCtor.Create();               //创建编辑器组件构造器实例
    WOrkSpaceMgrCtor.CreateAWorkSpaceMgr(g_WorkSpaceMgr, g_MainForm.pnl_WorkSpace);    //创建工作区管理器实例
    WorkSpaceMgrCtor.Free();        //使用工作区管理器构造器实例创建完工作区管理器实例之后，就可以将工作区管理器构造器释放了

    g_InterActive := TssnInterActive.Create(g_MainForm.Handle);//用户交互类的实例创建，参数是：主窗体句柄

    g_MainForm.Init();

    Result := 1;
end;

{释放在InitObjects方法中创建的实例}
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
