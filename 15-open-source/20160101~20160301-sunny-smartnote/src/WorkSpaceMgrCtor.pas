////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   WorkSpaceMgrCtor.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit WorkSpaceMgrCtor;

{���ڹ������ĸ������ϸ�μ� EditorCtor ��Ԫ
  ����������濴��Delphi����ͻ�ơ�
  }

interface

uses Controls,
     WorkSpaceMgr;

type
    {TssnWorkSpaceMgrCtor��Ϊһ��ֻ��һ�������鷽���ĳ�����
      û���κ�ʵ�ִ���
      ��������һ������������������������������ʵ��
      }
    TssnWorkSpaceMgrCtor = class
    public
        function CreateAWorkSpaceMgr(var WorkSpaceMgr : TssnWorkSpaceMgr; ParentCtrl : TWinControl) : Integer; virtual; abstract;
    end;

    {TssnTabWorkSpaceMgrCtor��TssnTabWorkSpaceMgr�Ĺ�����
      }
    TssnTabWorkSpaceMgrCtor = class(TssnWorkSpaceMgrCtor)
    public
        function CreateAWorkSpaceMgr(var WorkSpaceMgr : TssnWorkSpaceMgr; ParentCtrl : TWinControl) : Integer; override;
    end;

implementation

uses TabWorkSpaceMgr;

{ TssnTabWorkSpaceMgrCtor }

{����ʵ�֣����������ﴴ��TssnTabWorkSpaceMgr��ʵ��
  }
function TssnTabWorkSpaceMgrCtor.CreateAWorkSpaceMgr(var WorkSpaceMgr: TssnWorkSpaceMgr; ParentCtrl: TWinControl): Integer;
begin
    WorkSpaceMgr := TssnTabWorkSpaceMgr.Create(ParentCtrl);
    Result := Integer(WOrkSpaceMgr <> nil);
end;

end.
