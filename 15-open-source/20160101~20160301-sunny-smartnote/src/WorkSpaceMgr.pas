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

{˵������Ϊ�ڡ�Delphi����ͻ�ơ�һ�����жԱ���Ԫ�ĸ�Ϊ��ϸ�Ľ���
  �����ڱ�pas��ע�͵�˵�����ǲ���ȫ��
  ���Կ���pas��Դ�룬һ��ҪŶ������е���Ӧ�½�
  }
unit WorkSpaceMgr;

interface

uses controls, dialogs, SysUtils,
     ssnPublic, WorkSpace;

type
    {��������Sunny SmartNote��һ���๤�����Ĵ��ı��༭�����
      �����Ҫ�Թ������Ĺ��������г�����������ά�����������еĹ�����
      ���½����ر�һ������������ȡ��ǰ����Ĺ�������
      ����������ģ��Ĵ���Ϳ��Դӷ����Ĺ������������н��ѳ�����
      ��ȻҲ������TssnWorkSpaceMgr��
      }

    TssnWorkSpaceStatusRec = record
        Opened : Boolean;
    end;

    {TssnWorkSpaceMgr���TssnWorkSpace��TssnEditorһ����Ҳ��һ�������ࡣ
      ������������Ϊһ������������������������Ľӿڡ�
      ����ʵ��ÿһ�ֽ�����ġ������������������඼�Ǵ�TssnWorkSpaceMgr����
      �Դ˽�������������������ʵ�ֺͽӿڷ��뿪
      �ӿ��ǲ���ģ�ʵ�ֿ����������
      }
    {���⣬������������������ô��������ľ���ʵ�֣�Ҳ���롰���������Ľ������ʵ�����
      ���磬��MDI���ʵ�ֶ๤��������ô�๤��������������һ�����Ӵ��ڹ���������ά��ÿһ���򿪵ġ����������Ӵ���
      ����÷�ҳ���TPageControl����ʵ�ֶ๤��������ô�������������Ͷ�Ӧһ��TPageControl������������ÿ���������Ͷ�Ӧ��һ��TTabSheet���
     }
    {TssnWorkSpaceMgr��TssnWorkSpace�Ĺ�ϵ��һ�Զ�Ĺ�ϵ
      }
    TssnWorkSpaceMgr = class
    private
        m_OpenedCount : Integer;

    protected

      {��Ϊ�๤�����Ĺ�������TssnWorkSpaceMgr������ʶ��֪�����еĹ���������ά�����ǵ�״̬�����TssnWorkSpaceMgrӵ�������б����������б��״̬�б�
        Ϊ��Ч�����⣬������TssnWorkSpaceMgr�䱸һ���Ѿ��򿪹��������������ݳ�Ա�����⵱��Ҫ����ֵʱ�š���ʱ����š���æȥ�������
      ���⣬���ڹ������б��״̬�б���ܻᱻTssnWorkSpaceMgr�����������õ�������������Ҫ֪��ĳһ������״̬ʱ������˽������б������protected��
        }
        {SSN_MAX_WORKSPACE   = 128;
          ��������ͬʱ�򿪵Ĺ�������������Ŀ��128
          }
        m_WorkList : array [1 .. SSN_MAX_WORKSPACE] of TssnWorkSpace;

        {TssnWorkSpaceStatusRec = record    ���ڱ�־������״̬���Ѵ򿪻����ѹرգ�
          Opened : Boolean;
        end;
        ����״̬��m_WorkStatus��m_WorkListһһ��Ӧ
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

{�ر�ĳ��������}
function TssnWorkSpaceMgr.CloseWorkSpace(nIndex : Integer): Boolean;
var
    i : Integer;
begin
    Result := false;

    if nIndex = 0 then
        Exit;
    if not m_WorkStatus[nIndex].Opened then
        Exit;

    if m_WorkList[nIndex].Close() <> 0 then   {����رչ������ɹ�����û�б��û�ȡ����}
    begin
        Result := true;
        m_WorkList[nIndex].Free();
        m_WorkList[nIndex] := nil;
        m_WorkStatus[nIndex].Opened := false;
        Dec(m_OpenedCount);

        {�رյ�ǰ��������Ѱ����һ��������Ĺ�����}
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

    {�¼�ί����صĴ���}
    g_WorkSpaceEvent.OnWorkSpaceOpenClose(nil);
end;

{�½�һ����������ʵ��
  ���ͻ������½�һ��������ʱ�����Ȳ��ҿ��й�����������ҵ��򴴽������������󣬲�������
  ����ֵΪ�½���������������
  ���û���ҵ��򷵻�0
  ������������һ�������������ǵ���DoNewWorkSpace()��ɵ�
    DoNewWorkSpace()�ٴ˱�����Ϊ�����鷽������Ҫ�̳���TssnWorkSpaceMgr������ʵ��
  }
function TssnWorkSpaceMgr.NewWorkSpace(FileName : String): Integer;
var
    i : Integer;
begin
    Result := 0;

    {Ѱ��һ����δ�򿪡������еģ�������}
    for i := 1 to SSN_MAX_WORKSPACE do
    begin
        if not m_WorkStatus[i].Opened then
        begin
            Result := i;
            break;
        end;
    end;

    {����ҵ�}
    if Result <> 0 then
    begin
        m_WorkStatus[i].Opened := true;               //����״̬��ΪTrue

        {����������Ĵ����µĹ������ķ�������TssnWorkSpaceMgr����Ϊ�����鷽������������ʵ�֣����Կ�������TssnTabWorkSpaceMgr�����������ôʵ�ֵģ����������}
        DoNewWorkSpace(m_WorkList[i], FileName, i);
        ActiveWorkSpace(i);                           //���ǰ������
        Inc(m_OpenedCount);
    end;

    {�¼�ί����صĴ���}
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

{��ȡ��ǰ����������
  GetActiveWorkSpace��GetActiveWorkSpaceIndex������������������Ҫ
  �ڴ�ѡ��GetActiveWorkSpace��������Public����
  ���GetActiveWorkSpace������GetActiveWorkSpaceIndex��ʵ��
  ���㷨�����Ȼ�ȡ��ǰ����Ĺ������������ţ�Ȼ��ͨ���������ڹ������б��ҵ������Ĺ���������}
function TssnWorkSpaceMgr.GetActiveWorkSpace: TssnWorkSpace;
var
    nActive : Integer;
begin
    Result := nil;
    nActive := GetActiveWorkSpaceIndex();   //�Ȼ�ȡ��ǰ����Ĺ����������ţ����ǳ����鷽������������ʵ��
    if nActive = 0 then
        Exit;
    Result := m_WorkList[nActive];          //Ȼ��ͨ���������ڹ������б��ҵ������Ĺ���������
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
