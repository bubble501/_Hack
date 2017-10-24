////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   WorkSpaceMgr.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

{说明：因为在《Delphi高手突破》一书中有对本单元的更为详细的解释
  而我在本pas中注释的说明还是不够全面
  所以看此pas的源码，一定要哦结合书中的相应章节
  }
unit WorkSpaceMgr;

interface

uses controls, dialogs, SysUtils,
     ssnPublic, WorkSpace;

type
    {根据需求，Sunny SmartNote是一个多工作区的纯文本编辑软件。
      因此需要对工作区的管理器进行抽象，由它负责维护、管理所有的工作区
      如新建、关闭一个工作区，获取当前激活的工作区等
      这样，界面模块的代码就可以从繁琐的工作区管理工作中解脱出来了
      自然也就有了TssnWorkSpaceMgr类
      }

    TssnWorkSpaceStatusRec = record
        Opened : Boolean;
    end;

    {TssnWorkSpaceMgr如果TssnWorkSpace、TssnEditor一样，也是一个抽象类。
      它给出的是作为一个“工作区管理器”所必需的接口。
      具体实现每一种界面风格的“工作区管理器”的类都是从TssnWorkSpaceMgr派生
      以此将“工作区管理器”的实现和接口分离开
      接口是不变的，实现可以随意更改
      }
    {另外，“工作区管理器”的么个界面风格的具体实现，也是与“工作区”的界面风格的实现相关
      比如，用MDI风格实现多工作区，那么多工作区管理器就是一个“子窗口管理器”，维护每一个打开的“工作区”子窗口
      如果用分页风格（TPageControl）来实现多工作区，那么工作区管理器就对应一个TPageControl组件，而具体的每个工作区就对应于一个TTabSheet组件
     }
    {TssnWorkSpaceMgr与TssnWorkSpace的关系是一对多的关系
      }
    TssnWorkSpaceMgr = class
    private
        m_OpenedCount : Integer;

    protected

      {作为多工作区的管理器，TssnWorkSpaceMgr必须认识、知道所有的工作区，并维护它们的状态。因此TssnWorkSpaceMgr拥有两个列表——工作区列表和状态列表
        为了效率问题，决定给TssnWorkSpaceMgr配备一个已经打开工作区总数的数据成员，以免当需要该数值时才“临时抱佛脚”匆忙去计算出来
      另外，由于工作区列表和状态列表可能会被TssnWorkSpaceMgr的派生类所用到（如派生类需要知道某一工作区状态时），因此将两个列表放置在protected中
        }
        {SSN_MAX_WORKSPACE   = 128;
          所以允许同时打开的工作区的做多数目是128
          }
        m_WorkList : array [1 .. SSN_MAX_WORKSPACE] of TssnWorkSpace;

        {TssnWorkSpaceStatusRec = record    用于标志工作区状态（已打开还是已关闭）
          Opened : Boolean;
        end;
        工作状态，m_WorkStatus和m_WorkList一一对应
          }
        m_WorkStatus : array [1 .. SSN_MAX_WORKSPACE] of TssnWorkSpaceStatusRec;

        function DoNewWorkSpace(var WorkSpace : TssnWorkSpace; FileName : String; nIndex : Integer) : Integer; virtual; abstract;
        procedure DoActiveWorkSpace(nIndex : Integer); virtual; abstract;
        function GetActiveWorkSpaceIndex() : Integer; virtual; abstract;

    public
        constructor Create();
        destructor Destroy(); override;

        function NewWorkSpace(FileName : String) : Integer;
        function CloseWorkSpace(nIndex : Integer) : Boolean;
        procedure ActiveWorkSpace(nIndex : Integer);
        function GetWorkSpace(nIndex : Integer) : TssnWorkSpace;
        function GetWorkSpaceCount() : Integer;

        function CloseAll() : Boolean;
        function SaveAll() : Boolean;

        function ActiveNextWorkSpace() : Integer;
        function GetActiveWorkSpace() : TssnWorkSpace;
    end;

implementation

uses GlobalObject;

{ TssnWorkSpaceMgr }

constructor TssnWorkSpaceMgr.Create();
var
    i : Integer;
begin
    for i := 1 to SSN_MAX_WORKSPACE do
    begin
        m_WorkList[i] := nil;
        m_WorkStatus[i].Opened := false;
    end;

    m_OpenedCount := 0;
end;

destructor TssnWorkSpaceMgr.Destroy();
var
    i : Integer;
begin
    for i := 1 to SSN_MAX_WORKSPACE do
    begin
        if m_WorkStatus[i].Opened then
        begin
            m_WorkList[i].Free();
            m_WorkList[i] := nil;
            m_WorkStatus[i].Opened := false;
        end;
    end;
end;

{关闭某个工作区}
function TssnWorkSpaceMgr.CloseWorkSpace(nIndex : Integer): Boolean;
var
    i : Integer;
begin
    Result := false;

    if nIndex = 0 then
        Exit;
    if not m_WorkStatus[nIndex].Opened then
        Exit;

    if m_WorkList[nIndex].Close() <> 0 then   {如果关闭工作区成功（即没有被用户取消）}
    begin
        Result := true;
        m_WorkList[nIndex].Free();
        m_WorkList[nIndex] := nil;
        m_WorkStatus[nIndex].Opened := false;
        Dec(m_OpenedCount);

        {关闭当前工作区后，寻找下一个被激活的工作区}
        for i := nIndex - 1 downto 1 do
        begin
            if m_WorkStatus[i].Opened then
            begin
                ActiveWorkSpace(i);
                Exit;
            end;
        end;

        for i := nIndex + 1 to SSN_MAX_WORKSPACE do
        begin
            if m_WorkStatus[i].Opened then
            begin
                ActiveWorkSpace(i);
                Exit;
            end;
        end;
    end;

    {事件委托相关的代码}
    g_WorkSpaceEvent.OnWorkSpaceOpenClose(nil);
end;

{新建一个工作区的实现
  当客户请求新建一个工作区时，首先查找空闲工作区，如果找到则创建出工作区对象，并激活它
  返回值为新建工作区的索引号
  如果没有找到则返回0
  其中真正创建一个工作区对象，是调用DoNewWorkSpace()完成的
    DoNewWorkSpace()再此被定义为抽象虚方法，需要继承自TssnWorkSpaceMgr的类来实现
  }
function TssnWorkSpaceMgr.NewWorkSpace(FileName : String): Integer;
var
    i : Integer;
begin
    Result := 0;

    {寻找一个“未打开”（空闲的）工作区}
    for i := 1 to SSN_MAX_WORKSPACE do
    begin
        if not m_WorkStatus[i].Opened then
        begin
            Result := i;
            break;
        end;
    end;

    {如果找到}
    if Result <> 0 then
    begin
        m_WorkStatus[i].Opened := true;               //将其状态置为True

        {这才是真正的创建新的工作区的方法，在TssnWorkSpaceMgr中设为抽象虚方法，由其子类实现，可以看其子类TssnTabWorkSpaceMgr里面具体是怎么实现的，以深入理解}
        DoNewWorkSpace(m_WorkList[i], FileName, i);
        ActiveWorkSpace(i);                           //激活当前工作区
        Inc(m_OpenedCount);
    end;

    {事件委托相关的代码}
    g_WorkSpaceEvent.OnWorkSpaceOpenClose(nil);
end;

procedure TssnWorkSpaceMgr.ActiveWorkSpace(nIndex: Integer);
begin
    if nIndex = 0 then
        Exit;
    if not m_WorkStatus[nIndex].Opened then
        Exit;
    DoActiveWorkSpace(nIndex);
end;

{获取当前工作区对象
  GetActiveWorkSpace和GetActiveWorkSpaceIndex两个方法互补，都需要
  在此选择GetActiveWorkSpace方法放在Public里面
  因此GetActiveWorkSpace依赖于GetActiveWorkSpaceIndex来实现
  其算法就是先获取当前激活的工作区的索引号，然后通过索引号在工作区列表找到真正的工作区对象}
function TssnWorkSpaceMgr.GetActiveWorkSpace: TssnWorkSpace;
var
    nActive : Integer;
begin
    Result := nil;
    nActive := GetActiveWorkSpaceIndex();   //先获取当前激活的工作区索引号，它是抽象虚方法，有子类来实现
    if nActive = 0 then
        Exit;
    Result := m_WorkList[nActive];          //然后通过索引号在工作区列表找到真正的工作区对象
end;

function TssnWorkSpaceMgr.GetWorkSpace(nIndex : Integer): TssnWorkSpace;
begin
    Result := nil;
    if m_WorkStatus[nIndex].Opened then
        Result := m_WorkList[nIndex];
end;

function TssnWorkSpaceMgr.GetWorkSpaceCount: Integer;
begin
    Result := m_OpenedCount;
end;

function TssnWorkSpaceMgr.CloseAll: Boolean;
var
    i : Integer;
begin
    Result := false;
    for i := 1 to SSN_MAX_WORKSPACE do
    begin
        if not m_WorkStatus[i].Opened then
            continue;
        if not CloseWorkSpace(i) then
            Exit;
    end;
    Result := true;
end;

function TssnWorkSpaceMgr.SaveAll: Boolean;
var
    i : Integer;
begin
    Result := false;
    for i := 1 to SSN_MAX_WORKSPACE do
    begin
        if not m_WorkStatus[i].Opened then
            continue;
        if not m_WorkList[i].Save() then
            Exit;
    end;
    Result := true;
end;

function TssnWorkSpaceMgr.ActiveNextWorkSpace: Integer;
var
    i : Integer;
begin
    Result := GetActiveWorkSpaceIndex();

    if m_OpenedCount <= 1 then
        Exit;

    for i := Result + 1 to SSN_MAX_WORKSPACE do
    begin
        if not m_WorkStatus[i].Opened then
            continue;
        ActiveWorkSpace(i);
        Result := i;
        Exit;
    end;

    for i := 1 to Result - 1 do
    begin
        if not m_WorkStatus[i].Opened then
            continue;
        ActiveWorkSpace(i);
        Result := i;
        Exit;
    end;
end;

end.
