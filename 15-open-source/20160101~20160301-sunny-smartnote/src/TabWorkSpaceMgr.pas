////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   TabWorkSpaceMgr.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit TabWorkSpaceMgr;

interface

uses comctrls, Classes, ExtCtrls, Controls, Sysutils,
     WorkSpaceMgr, WorkSpace, MultiLan;

{�������ʵ����TssnWorkSpace��TssnWorkSpaceMgr�����Ƕ��ǳ����࣬������ֱ�Ӵ��������ǵ�ʵ����
  ������Ҫ�ȴ����ǵ�������������������ʵ��ĳһ�๤�����Ľ�����

  ����TssnTabWorkSpaceMgr��TssnTabWorkSpace��ʵ���ϵĹ����ԣ����Խ�����������ʵ�ַ���һ����Ԫ�ļ�����
    ������ô������������ĸ���Ԫ�ļ����涼���н�����
    �������Է����������������
  }

{Ŀǰ������ʱʹ�÷�ҳ���û�����ʵ�ֶ๤����
  ��ˣ����ݶ�ҳ�������TPageControl��Ҫ�����Ǵ�TssnWorkSpace����ʵ����TssnTabWorkSpace
  ��TssnWorkSpaceMgr����ʵ����TssnTabWorkSpaceMgr

��Ȼ������ҲҪ�������ʱ�����๤������ʵ�֣���ôֻ��Ҫ�ٴ�TssnWorkSpace��TssnWorkSpaceMgr������һ���µĽ�����ʵ�ּ���
  }

type
    {TssnTabWorkSpaceMgr������TssnWorkSpaceMgr����ְ������TPageControl���ʵ��һ����ʵ�����Ĺ�����������
      ���TssnTabWorkSpaceMgrֻ��Ҫ�ڲ�����һ��TPageControl��ʵ������ʵ��TssnWorkSpaceMgr�����ĳ����鷽������
      }
    TssnTabWorkSpaceMgr = class(TssnWorkSpaceMgr)
    private
        m_Tab : TPageControl;

    protected
        {��ϻ����NewWorkSpace()���Template Methodģʽ��DoNewWorkSpace
          }
        function DoNewWorkSpace(var WorkSpace : TssnWorkSpace; FileName : String; nIndex : Integer) : Integer; override;

        {���ActiveWorkSpace��DoActiveWorkSpace
          }
        procedure DoActiveWorkSpace(nIndex : Integer); override;

        {��ȡ��ǰ��������ı�ʶ��������ֵ��
          ��ֻ�ǰ���ÿ��TTabSheet�����tag�����е�ֵȡ��������
          }
        function GetActiveWorkSpaceIndex() : Integer; override;

    public
        {�乹�캯����һ��ParentCtrl��������ָ��TPageControl�����������һ�������
          �ڹ��캯������ɶ�TPageControlʵ��--m_Tab�Ĵ����ͳ�ʼ������
          }
        constructor Create(ParentCtrl : TWinControl);
        destructor Destroy(); override;

    end;

    {TssnTabWorkSpace������TssnWorkSpace�����߼��ϵ�ְ���Ǿ���ʵ��һ���������ķ��
      �๤�����������Ѿ�ȷ��ʹ�÷�ҳ���TPageControl��ʵ�֣�TPageControl�����ʵ�Ƿ�ҳ�е�ÿһҳ--TTabSheet������
      ÿ����һ��ҳ��Ҳ���Ǵ�����һ��TTabSheet��ʵ��
      TPageControl��TTabSheet�Ĺ�ϵ����TssnWorkSpaceMgr��TssnWorkSpace�Ĺ�ϵ�Ƕ�ô����ѽ
      ����Ȼ��ʵ���ϣ�TssnTabWorkSpace������������Ӧ��TTabSheet��TssnTabWorkSpaceMgr������������������Ӧ��TPageControl
      }
    TssnTabWorkSpace = class(TssnWorkSpace)
    private
        m_TabSheet : TTabSheet;
        procedure SetCaption(FileName : String);  //Ϊÿ��TTabSheet��ʵ������һ������

    protected
        procedure OnSave(); override;

    public
        {Mgrָ�����������������һ��TPageControl
         FileName������������Ҫ�򿪵��ļ������ޱ��⹤������ָ���ļ���Ϊ''
         nIndex�ǹ�����������������¹������ı�ʶ��}
        constructor Create(Mgr : TPageControl; FileName : String; nIndex : Integer);
        destructor Destroy(); override;

        {��ȡPageControl������ֵ
          ������ֵ�롰���������������ֵ��ͬ

          ��Ҫ�����������ʵ�������--TssnTabWorkSpaceMgr�ṩÿ��TTabSheetʵ����TPageControl�е�����ֵ
            ������ֵ��Ϊÿ������������ı�ʶ����ͬ���������ı�ʶ����ʵ��һ���߼��ϵķ��ţ���ţ�
            ����TTabSheet��TPageControl�е�����ֵ�ǲ�ͬ��
            ������ֵ��TPageControl�ڹ�����TTabSheetʱ��ʹ��
          }
        function TabWS_GetPageIndex() : Integer;
    end;

implementation

uses GlobalObject;

{ TssnTabWorkSpaceMgr }
{�乹�캯����һ��ParentCtrl��������ָ��TPageControl�����������һ�������
  �ڹ��캯������ɶ�TPageControlʵ��--m_Tab�Ĵ����ͳ�ʼ������
  }
constructor TssnTabWorkSpaceMgr.Create(ParentCtrl: TWinControl);
begin
    inherited Create();

    m_Tab := TPageControl.Create(ParentCtrl);
    m_Tab.Parent := ParentCtrl;
    m_Tab.Align := alClient;
    m_Tab.Visible := true;

    {���¼������й�}
    m_Tab.OnChange := g_WorkSpaceEvent.OnWorkSpaceChange;
end;

destructor TssnTabWorkSpaceMgr.Destroy;
begin
    inherited;

    m_Tab.Free();
    m_Tab := nil;
end;

function TssnTabWorkSpaceMgr.DoNewWorkSpace(var WorkSpace: TssnWorkSpace; FileName : String; nIndex : Integer): Integer;
begin
    {����һ���µĹ�����
      ������һ���µ�TssnTabWorkSpaceʵ����TTabSheet�����}
    WorkSpace := TssnTabWorkSpace.Create(self.m_Tab, FileName, nIndex);
    
    Result := Integer(WorkSpace <> nil);
end;

procedure TssnTabWorkSpaceMgr.DoActiveWorkSpace(nIndex: Integer);
begin
    //����һ��������
    m_Tab.ActivePageIndex := TssnTabWorkSpace(m_WorkList[nIndex]).TabWS_GetPageIndex;
end;

{��ȡ��ǰ��������ı�ʶ��������ֵ��
  ��ֻ�ǰ���ÿ��TTabSheet�����tag�����е�ֵȡ��������
  }
function TssnTabWorkSpaceMgr.GetActiveWorkSpaceIndex: Integer;
begin
    Result := 0;
    if m_Tab.PageCount = 0 then
        Exit;

    //���ǵ��ڴ���ÿһ��TTabSheetʱ����������ֵ�����������tag��������    
    Result := m_Tab.ActivePage.Tag;
end;

{ TssnTabWorkSpace }

{Mgrָ�����������������һ��TPageControl
  FileName������������Ҫ�򿪵��ļ������ޱ��⹤������ָ���ļ���Ϊ''
  nIndex�ǹ�����������������¹������ı�ʶ��}
constructor TssnTabWorkSpace.Create(Mgr : TPageControl; FileName : String; nIndex : Integer);
begin
    //���ȴ���TTabSheet��ʵ������ָ����PageControl����
    m_TabSheet := TTabSheet.Create(nil);
    m_TabSheet.PageControl := Mgr;
    
    m_TabSheet.Align := alClient;
    m_TabSheet.Visible := true;

    //����TTabSheet�����tag���ԣ���ס�ù��������߼���ʶ��
    m_TabSheet.Tag := nIndex;

    //���ñ��⣬�����û���Ĺ��캯��
    SetCaption(FileName);
    inherited Create(m_TabSheet, FileName, nIndex);
end;

destructor TssnTabWorkSpace.Destroy;
begin
    inherited;

    m_TabSheet.Free();
    m_TabSheet := nil;
end;

procedure TssnTabWorkSpace.OnSave;
begin
    SetCaption(m_Editor.GetFileName());
end;

procedure TssnTabWorkSpace.SetCaption(FileName : String);
begin
    if FileName = '' then
        m_TabSheet.Caption := str_NoTitle
    else
        m_TabSheet.Caption := System.Copy(ExtractFileName(FileName), 1, 10);
end;

function TssnTabWorkSpace.TabWS_GetPageIndex: Integer;
begin
    Result := m_TabSheet.PageIndex;
end;

end.
