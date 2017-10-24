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

{关于构造器的概念，请详细参见 EditorCtor 单元
  另外就是认真看《Delphi高手突破》
  }

interface

uses Controls,
     WorkSpaceMgr;

type
    {TssnWorkSpaceMgrCtor作为一个只有一个抽象虚方法的抽象类
      没有任何实现代码
      真正创建一个工作区管理器的任务由其派生类实现
      }
    TssnWorkSpaceMgrCtor = class
    public
        function CreateAWorkSpaceMgr(var WorkSpaceMgr : TssnWorkSpaceMgr; ParentCtrl : TWinControl) : Integer; virtual; abstract;
    end;

    {TssnTabWorkSpaceMgrCtor是TssnTabWorkSpaceMgr的构造器
      }
    TssnTabWorkSpaceMgrCtor = class(TssnWorkSpaceMgrCtor)
    public
        function CreateAWorkSpaceMgr(var WorkSpaceMgr : TssnWorkSpaceMgr; ParentCtrl : TWinControl) : Integer; override;
    end;

implementation

uses TabWorkSpaceMgr;

{ TssnTabWorkSpaceMgrCtor }

{代码实现：就是在这里创建TssnTabWorkSpaceMgr的实例
  }
function TssnTabWorkSpaceMgrCtor.CreateAWorkSpaceMgr(var WorkSpaceMgr: TssnWorkSpaceMgr; ParentCtrl: TWinControl): Integer;
begin
    WorkSpaceMgr := TssnTabWorkSpaceMgr.Create(ParentCtrl);
    Result := Integer(WOrkSpaceMgr <> nil);
end;

end.
